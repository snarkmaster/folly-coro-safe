## Start here

This document is for anyone who writes significant amounts of asynchronous
code using `folly::coro`, and *especially* for people whose use-case does
not fit perfectly into the [C++ structured concurrency](
https://ericniebler.com/2020/11/08/structured-concurrency/) paradigm.

You are especially encouraged to read this if you require tasks that are
**not** immediately awaited, such as deferred or background tasks.

This document is meant to be the entry point to the APIs and programming
model of `folly/coro/safe`.  It assumes familiarity with `coro/README.md`,
with structured concurrency, and awareness (but not expert knowledge) of
these `folly::coro` constructs:
  - `Task` -- a lazy-start coroutine representing async work,
  - `AsyncScope` -- a collection of background `Task`s or other awaitables.

You may ignore references to edge-case tools like `co_invoke` or
`co_scope_exit` -- these are mentioned by way of comparison, but this
document argues you should avoid them in favor of `coro/safe` APIs.

## Problems solved (and not solved)

Straight-up structured concurrency is almost as "safe" as regular C++, with
some new risks relating to multi-threading, e.g.
  - Deadlock by holding a non-`coro` mutex across a suspension point.
  - Deadlock due to a blocking wait on a thread that must suspend in order
    to complete the awaited task (e.g. blocking wait in a destructor).

At present, `coro/safe` does **not** attempt to improve thread- or
deadlock-safety, and focuses instead on memory-safety, exception-safety, and
cancellation-correctness.

`coro/safe` specifically addresses the fact that `folly::coro` tasks that
aren't awaited immediately are vulnerable to a host of new bugs.  Some of
the greatest hits:
  - Task outlives its lambda with captures -- guaranteed dangling refs.
  - Task takes inputs by reference (params, captures, other aliasing) --
    the references can easily become dangling, especially with `AsyncScope`
    and unhandled exceptions.
  - Member coroutines can accidentally outlive `this`.  This dangling ref
    bug is especially easy to make if the object is made in a loop.
  - Failing to join `AsyncScope` due to an unhandled exception.  This issue
    is pervasive, since C++ has no RAII for `co_await` -- but `coro/safe`
    offers a workable emulation.
  - Incorrect cancellation setup for `AsyncScope` tasks.

For a closer look at some of these footguns, check out Francesco Zoffoli's
[CppCon 2023 talk](https://www.youtube.com/watch?v=Iqrd9vsLrak) or [slides](
https://github.com/CppCon/CppCon2023/blob/main/Presentations/coroutine_patterns.pdf).
While the talk recommends `co_invoke` and a manually-invoked `cleanup()`
member coro, you will see that `coro/safe` offers more robust solutions.

## How does it work?

### Compile-time reference safety

When you use `coro/safe` constructs, you end up telling the compiler -- through
the type system -- a fairly precise lifetime contract for the objects you are
referencing (details in `SafeAlias.h` and `Captures.h`).  As a result, many
common lifetime bugs become compiler errors.  Briefly, when you intact with
"safe" tasks like `async_closure` and friends:
  - Value types like `int` maybe passed everywhere without restriction.
  - Child coros can refer to data owned by parent closures via wrapper
    classes from `Captures.h`.  While there's a zoo of `capture` sub-types,
    in typical usage, you should take them by `auto`.  They can wrap value &
    reference types, and support the common cvref categories.  The wrapper
    "commutes" with taking references or adding `const`.  All `capture`
    classes expose the underlying type via a pointer-like interface, and
    only `capture_nullable` is nullable.
  - `AsyncObject`s are typically owned via `AsyncObjectPtr`, which resembles
    `co_cleanup_capture`, but with nullability, and necessarily weaker
    lifetime safety.
  - You may never pass (without `manual_safe_ref*`, below) references,
    pointers, reference wrappers, and anything containing them.  Caveat: Due
    to C++ limitations, we only introspect a few `std::` containers, and
    cannot detect aliasing within generic classes.
  - If the type heuristics are too limiting, but you can prove your usage is
    safe, wrap a value in `manual_safe_ref*` for a simple escape hatch.
    **Always** add a comment explaining why it's safe!

### Async RAII

`coro/safe` constructs like `async_closure()` and `AsyncObject` provide a strong
form of async RAII:
  - `co_cleanup(async_closure_private_t)` is guranteed to be invoked. Details in
    `CoCleanupAsyncRAII.md`.
  - Cleanup invocation is exception-safe, handling both errors in the "inner
    scope" and from sibling data with `co_cleanup`.
  - Both "closure body" and "cleanup" exceptions propagate to the caller.
    There is no channel for multiple errors, so one will be picked
    arbitrarily.  Currently (this is not a contract), any "inner" error
    takes priority over "cleanup" ones, since it's more likely to be the
    root cause.
  - Unlike synchronous RAII, cleanup of sibling object is currently unordered,
    to potentially later support concurrent cleanup of unrelated objects.
  - The RAII implementation integrates with `SafeAlias.h` type annotations
    to ensure that `co_cleanup` calls do not access the data belonging to
    siblings with `co_cleanup` -- they could have been cleaned up.
  - You can return closure-owned data outside of the closure via
    `AfterCleanup.h`.

### Safer background tasks via `SafeAsyncScope`

`SafeAsyncScope.h` addresses all the major problems with `AsyncScope`:
  - Auto-join & exception-safety via `co_cleanup` async RAII.
  - Refs taken by background tasks are lifetime-safe due to `SafeAlias.h`.
  - `AsyncScopeObject.h` helps avoid the following commmon, deadlock-prone
    antipattern: "My object has an `AsyncScope`, so I'll do a blocking wait
    to join it in the destructor." Instead, you will delegate your object's
    cleanup to a parent `SafeAsyncScope`.
  - Cancellation is wired up correctly (and customizably) by default.
  - Source location & async stack propagation is correct by default.
  - Scope coros can schedule other coros on the same scope via the
    memory-safe APIs `scheduleScopeClosure()` and (for `AsyncObject`s)
    `scheduleSelfClosure()`.

## `coro/safe` cheat sheet

All the familiar `folly::coro` concepts will continue to apply, but a few
things need to be expressed differently.

  * `Task` -> `ClosureTask`, `MemberTask`, `ValueTask`, `CoCleanupSafeTask`,
    `AutoSafeTask`, `NowTask`.  Yes, this is a mouthful, and you will want
    to review `APIBestPractices.md` to understand the various use-cases.
    However, basic usage is simple:
      - Immediately awaited tasks: `NowTask`
      - Tasks taking only value-semantic args: `ValueTask`
      - Tasks to be wrapped by async closures:
          * Free or `static` functions - use `ClosureTask`.
          * Member function -- use `MemberTask`.
          * Optionally, wrap either kind with `as_async_closure` in an API
            prefixed with `ac_` to avoid `async_closure()` at each callsite.

  * `co_invoke`, `co_scope_exit`, lambda coros, async RAII -> instead, use
    `async_closure()`, documented in `AsyncClosure.md`.
      - Avoid `co_scope_exit` because `async_closure()` is a safer analog
        has strictly more functionality.  The big problem with
        `co_scope_exit` is it violates RAII expectations -- its async
        cleanup outlives the lexical scope that defines it.
      - Avoid `co_invoke` because `async_closure()` has similar safety
        benefits, but is designed for 0 runtime overhead in similar usage,
        and additionally supports async RAII.  Caveat: `co_invoke` is often
        used for lambda coros with captures, which can be more concise than
        the more efficient pass-by-value style.  If you need that use-case,
        check out `async_named_closure()` in "Future work".

  * For `AsyncScope` in a function, pass `safeAsyncScope<...>()` to
    an `async_closure()`.

  * Classes needing an `AsyncScope` member for background tasks --
    inherit `public`ly from `AsyncScopeObject<...>`.

  * Classes needing async destructors: `public`ly inherit from `AsyncObject`.

    Power users may want `AsyncScopeSlotObject`, or, in rare cases,
    to manually implement `co_cleanup()` -- read `CoCleanupAsyncRAII.md` first.

# Just show me some code!

Start with `folly/coro/safe/examples/CollectAllWindowedAsync.cpp`, which
demonstrates how a real-world scatter-gather utility becomes safe & legible
once migrated to `coro/safe` APIs.

The test `FooOnScope` in `AsyncScopeObjectTest.cpp` showcases a variety of
ways of scheduling background tasks from class member coros.

# Future work & contributions

Please discuss your contribution with Github user @snarkmaster. When
extending `coro/safe`, it is important to maintain some core invariants, and
this can require careful thought and code review. Some specific points:

  - We want to guarantee that any type needing an async destructor can
    **only** be created in a managed setting (i.e. `as_capture` or
    as an `AsyncObject::Slot`). Failing to enforce this constraint would
    break our core promise of "guaranteed async cleanup".

  - It requires attentive analysis of a use-case to define effective
    `SafeAlias.h` markings and `capture`-like pointers for new scenarios
    like `AsyncGenerator`.  For example -- arguments passed **into** a
    "safe" variant of generator should follow `async_closure` rules.  But,
    we'll need to think carefully about yielding references **from**
    generators, and having `co_yield` references return a reference **into**
    the generator (supported in Python, but not yet in `folly::coro`).

With that said, here are the things that we want to add:

**Generators:** The largest feature gap of `coro/safe` is that it lacks
extensions for `AsyncGenerator`.  Integrating this would be a finite,
well-defined project that **should** be built, given an impactful use-case.
Be sure to grep for breadcrumbs in the code.

**`async_named_closure`:** Since `async_closure()` deliberately disallows
lambda captures, it has a usability gap when passing many arguments by
value.  Luckily, this is easy to resolve by adding a keyword argument
syntax.  I have some WIP diffs for this, but it would take some focused work
to really polish it off:

```cpp
co_await async_named_closure(
    [](auto scope, auto kw) {
      scope.with(co_await co_current_executor).schedule(async_closure(
          [](auto kw2) {
            kw2["a"_id].fetch_add(kw["b"_id]);
            co_return;
          },
          kw));
      // NB: When adding `after_cleanup`, consider `co_after_cleanup` too.
      // Both would have `async_closure` argument semantics ...  and at
      // least the latter could perhaps even own its own `capture`s.
      // Also, the ideal syntax for this might be `.then()` or `operator|`
      // chaining of closure-like gadgets, with the `co_return` or `return`
      // of one being plumbed into the arguments of the next.
      co_return after_cleanup(
          [](auto kw2) { return kw2["a"_id].load(); },
          kw);
    },
    safeAsyncScope<CancelViaClosure>(),
    // Like in Python, kwargs must follow positional args, and are collected
    // into a single `auto kw` above.
    "a"_id = as_capture(make_in_place<std::atomic_int>(3)),
    "b"_id = 3);
```

**`restricted_co_cleanup_capture`:** Your will find mentions of "restricted"
`capture`s throughout the codebase.  That's an already-designed &
simple-to-implement feature meant to address a particular quirk of the
`capture` safety system.  Specifically, today, when you pass
`async_arc_cleanup<T&>` into a closure, this forces the data owned by the
closure to have weaker memory-safety markings.  That prevents the inner
closure from passing a reference to something it owns into a task scheduled
on the outer closure's scope (or other "cleanup" arg).  If you find yourself
inconvenienced by this quirk, it is likely that implementing restricted refs
can help -- a closure is unaffected when a restricted ref is passed into it,
and the restricted refs can still be used to schedule `ValueTask`s on the
corresponding scope / `co_cleanup` object.
