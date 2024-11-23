## Audience & motivation

You are familiar with `SaferCoro.md`, and you need more complex async patterns, e.g.
  - You cannot await every task in the statement that made it, so `NowTask`
    (pure structured concurrency) is insufficient.
  - You need to background tasks on a `SafeAsyncScope`. Perhaps, those tasks
    need to use `schedule*Closure()` to recursively schedule more on-scope tasks.
  - Some of your objects have require `co_cleanup()` async RAII. You've reviewed
    `CoCleanupAsyncRAII.md`, and understand the caveats.

## What is an async closure?

Think of `async_closure()` as a `Task<>` coroutine with extra features:
  - **Background task integration.** Just pass in
    `safeAsyncScope<CancelViaParent>()`.

    *(power users only)* If your custom `co_cleanup()` class needs to control
    the cancellation of other tasks, awaiting a closure that takes it
    `as_capture()` will call your class's `setParentCancelToken()` with the
    closure's own cancellation token.  This is done before awaiting the inner
    task.
  - **Compile-time memory safety.** `async_closure()` prevents these problems:
      - Passing shorter-lived references into background, or deferred tasks.
      - Returning references (including those in containers or tasks) that will
        have become dangling.

    In practice, safety is limited by the lack-of-reflection caveats at the top of `SafeAlias.h`, and usage of non-safe APIs (`manual_safe_ref`, `Task`, `AsyncScope`, etc). That said, here is what `async_closure()` does to help:
      - Checks `SafeAlias.h` constraints on its inputs. Captures
    & pass-by-reference are forbidden, so `co_invoke` is never needed.
      - Returns a `SafeTask` of the safety level implied by the arguments. Thus,
        returned tasks can be arguments to other safe tasks.
      - Owns its `as_capture()` arguments, which can be implicitly passed as
        `capture<>` references to sub-tasks and sub-closures. The refs bear
        safety markings designed to prevent lifetime bugs.
  - **Async RAII**, as in `CoCleanupAsyncRAII.md`. Unlike regular RAII, async
    cleanup order is unspecified, and potentially concurrent. Unlike
    `co_scope_exit`, memory safety bugs are largely avoided by clearer
    semantics, and `safe_alias_of_v checks.

If you are coming from Python, its `async with` is a close analog of "`async_closure()` + an `as_capture()` argument of a
`co_cleanup()`-enabled type". The analog of `__aexit__()` is `co_cleanup()`. There is no `__aenter__()` counterpart, since this can be trivially modeled with a wrapper task.

In exchange for the above safety features, your coroutine cannot be a lambda
with captures. Instead, pass your data by-value, or use `as_capture()` to pass
references from parent closures to children.

## Not all safe code needs `async_closure()`

Here is a short pyramid of safe primitives that complement closures.
`APIBestPractices.md` discusses them at length.
  - Most code should be `NowTask` or `ValueTask`.
  - Use `async_closure()` if your scope needs to defer awaiting tasks, or
    requires async RAII.
  - If your API needs `async_closure()` semantics, prefix it with `ac_` and use
    `as_async_closure()`.
  - If your object needs async RAII, use `AsyncScopeObject` or (more rarely)
    just `AsyncObject`.
  - Non-static member functions interacting with async closures or objects
    should be `MemberTask`.  Pass `capture<Obj&>` or `AsyncObjectPtr<Obj>` as the first arg to `async_closure(FOLLY_INVOKE_MEMBER(memberName), ...)`.
  - Generic code may benefit from `AutoSafeTask<T, SafetyArgs...>`.

## Example code

For code that actually works, your best bet is to peruse `AsyncClosureTest.cpp`,
`SafeAsyncScopeTest.cpp`, and the `examples/` subdir. But, to whet your
appetite, here's a small demo:

```cpp
assert(1337 == co_await async_closure([](auto scope, auto n)
      -> ClosureTask<move_after_cleanup<std::atomic_int, int>> {
    n->fetch_add(1000);
    // Immediate sub-task: takes `scope` and `n` by-reference.
    co_await async_closure([](auto scope, auto n) -> ClosureTask<void> {
      n->fetch_add(300);
      // Background task 1: `scheduleScopeClosure` lets it
      // safely schedule other sub-tasks on the same scope.
      auto* exec = co_await co_current_executor;
      scope->with(exec).scheduleScopeClosure(
          [](auto scope, auto n) -> ClosureTask<void> {
            n->fetch_add(30);
            // Background task 2, recursively scheduled by 1.
            auto* exec = co_await co_current_executor;
            scope->with(exec).schedule(async_closure(
                [](auto n) -> ClosureTask<void> {
                  n->fetch_add(7);
                  co_return;
                }), n);
          }, /*`scope` is implied, */ n);
    }, scope, n);
    // Atomics aren't movable, so convert to `int` after cleanup.
    co_return move_after_cleanup<std::atomic_int, int>(n);
  },
  safeAsyncScope<CancelViaParent>(),
  // Mutated by sub-tasks & returned after cleanup.
  as_capture(make_in_place<std::atomic_int>(0)));
```

## What are these `capture<>` things?

For the details, read `Captures.md` and `Captures.h`. Briefly, `capture`s are a family of type wrappers that:
  - Are made by passing `as_capture()` or `as_capture(make_in_place<T>())`
    args to an `async_closure()`.
  - Expose the underlying type via dereference, i.e. `*` and `->`.
  - Wrap both value and reference types.
      - Value types represent object owned by a closure. Their lifecycle is
        managed by the closure: closure's inner task `co_return` -> object `co_cleanup` -> object destructor.
      - Reference captures allow descendant tasks to safely reference an
        ancestor's data. This includes the natural implicit conversions. For
        example: `capture<int>` can be passed as `capture<int&>`; `const
        capture<T&>` quacks like `capture<const T&>.
      - An rvalue reference `capture` represent an ancestor wanting to pass
        ownership to a child. Consequently, the API of `capture<V&&> rv` follows
        the "destructive" style, so that use-after-move linters can be
        effective. For example, you would write `std::move(rv)->doSomething()`.
  - Specialize `safe_alias_of_v` so as to prevent specific lifetime safety
    bugs in the usage of async closures, tasks, and scopes.

When possible, use `auto` in signatures taking `capture`s. Code rarely cares what flavor of `capture` it has, and all of them dereference the same way.

There are two exceptions where the API isn't quite the same -- but even then, the differences aren't large enough to avoid `auto`.
  - `as_capture_unique()` special-cases passing `unique_ptr`s into captures, so
    that the closure code doesn't have to dereference twice. So, the
    `capture_unique<Value>` types are the only ones that are nullable.
  - `co_cleanup_capture<T>` types deliberately forbid rvalue reference `T`. You will
    probably never encounter this.

## Ordering: construction, cleanup, and destruction

  - `as_capture()` construction ordering is unspecified, since we use
    order-unspecified `std::tuple` to store `capture<>`s. This could be easily
    fixed by implementing a custom tuple-like storage, but the benefit seems
    small. After all, function argument order evaluation would still be
    unspecified, and (per the next bullet) it is not safe to create dependencies
    between `co_cleanup()` captures.
      - Future: If we really, really wanted specified ordering, an
        initializer-style closure flavor that chained multiple function calls to
        construct arguments could be built.
  - All `co_cleanup()` tasks can potentially run concurrently. You must
    explicitly sequence your cleanup steps via `co_await` if there are any data
    dependencies between them. Note: concurrent cleanup isn't currently
    implemented, and it would likely be an opt-in API, but the above remains best practice.
  - Destruction order is the opposite of the (currently unspecified)
    construction order.

## What does `async_closure()` cost at runtime?

Unlike `co_invoke()`, `async_closure()` is designed to be zero-cost if does not need to manage async RAII for any of its captures. It should literally compile down to "just passing its args to the inner task".

If you pass `co_cleanup()` arguments to the closure (like `safeAsyncScope<>()`),
then it has to create an additional "outer" coroutine frame to handle errors and
await cleanup. This cost should be roughly equivalent to that of
`co_scope_exit`, but much easier to use, and much harder to misuse.

## But what does it actually *do*?

In pseudo-C++, a closure with async RAII support looks like this:

```cpp
auto async_closure(auto make_inner_task, auto&&... args) {
  auto [boundArgs, captureStorage] =
      bind_closure_args(std::forward<Args>(args)...);
  // For `bad_alloc` safety, pre-make `co_cleanup()` tasks.
  // They may take a copy of `&captureStorage->closureError_`.
  auto cleanups = make_co_cleanup_tasks(captureStorage);
  return [](auto innerTask, auto cleanupTasks, auto storage)
      -> SafeTask<safe_alias_for_pack_<decltype(args)...>, auto> {
    static_assert(is_safe_task_v<decltype(innerTask)>);
    auto result;
    storage->closureError_ = try_and_catch([&] {
      apply_setParentCancelToken(storage->captureTuple_);
      result = co_await inner;
    });
    for (auto& cleanup : cleanups) {
      auto err = co_await co_awaitTry(std::move(cleanup));
      if (!storage->closureError_ && err) {
        storage->closureError_ = std::move(err);
      }
    }
    if (storage->closureError_) {
      co_yield co_error{std::move(storage->closureError_)};
    } else {
      co_return result;
    }
  }(
      make_inner_task(boundArgs...),
      std::move(cleanups),
      std::move(captureStorage));
}
```

Above, `boundArgs` may include three kinds of arguments:
  - `as_captures()` args, passed as `capture<V&>` refs pointing into
    `storage->captureTuple_`. These support `make_in_place<T>(...)`, which is
    useful for non-movable synchronization primitives like `coro::Baton`.
  - `capture<>`s from the parent, passed as implicitly-made `capture<V&>` refs.
  - Other "plain" args, passed as forwarding refs, and bound by value.

In practice, there is more going on than is shown above:
  - The "outer task" will be omitted if none of the `as_capture()` args have
    `co_cleanup` or `setParentCancelToken()`. In that setting `async_closure()`
    *should* have zero runtime cost. Without an "outer task", the net effect of
    `async_closure()` is to apply the argument binding logic, create the inner
    task, and re-wrap it as a `SafeTask` with the deduced safety level.
  - You can access `storage->captureTuple_` after cleanup using
    `AfterCleanup.h`. This is useful for moving a capture out into the parent,
    e.g.
  - The same machinery is reused for `AsyncObject` construction, `co_cleanup`,
    and destruction. It mainly differs in that it calls `setParentCancelToken`
    eagerly.
