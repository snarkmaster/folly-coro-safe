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

    *(power users only)* If your custom `as_capture()` class need to control the
    cancellation of other tasks, awaiting a closure will call its
    `setParentCancelToken()` with the closure's own cancellation token.  This is
    done before awaiting the inner task.
  - **Compile-time memory safety.** `async_closure()` checks `SafeAlias.h`
    constraints on its arguments -- pass-by-reference is forbidden, so
    `co_invoke` is never needed. It returns a `SafeTask` of the safety level
    implied by the arguments. `as_capture()` arguments are owned by the closure,
    and can be implicitly passed as `capture<>` references to sub-tasks and
    sub-closures. `safe_alias_of_v` markings on `capture` &
    `AsyncObjectPtr<Obj>` arguments reduce the risk of common lifetime bugs
    like:
      - Passing shorter-lived references into background tasks.
      - Returning references (including those in containers or tasks) that will
        have become dangling.
  - **Async RAII**, as in `CoCleanupAsyncRAII.md`. Unlike regular RAII, async
    cleanup order is unspecified, and potentially concurrent. Unlike
    `co_scope_exit`, memory safety bugs are largely avoided by clearer
    semantics, and `safe_alias_of_v checks.

If you are coming from Python, its `async with` is a close analog of "`async_closure()` + an `as_capture()` argument of a
`co_cleanup()`-enabled type". The analog of `__aexit__()` is `co_cleanup()`. There is no `__aenter__()` counterpart, since this can be trivially modeled with a wrapper task.

In exchange for the above safety features, your coroutine cannot be a lambda
with captures. Instead, pass your data by-value, or use `as_capture()` to pass
references from parent closures to children.

It *is* possible to safely use class member async closures, but their calling
convention is essentially that of a static function taking `capture<Object&>` as
the first argument. See `APIBestPractices.md` for the pattern.

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
    }
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
