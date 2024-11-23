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

To enforce pure structured concurrency in your code, `folly/coro/safe` encourages you to replace `Task` with these zero-cost wrappers:
  - `NowTask`: non-movable, only awaitable in the statement that made it, or
  - `ValueTask`: task is self-contained, contains no references to external data.

For more complex usage, read on.

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

### Compile-time memory safety

When you use `coro/safe` constructs, you end up telling the compiler -- through
the type system -- a fairly precise lifetime contract for the objects you are
referencing (details in `SafeAlias.h` and `Captures.h`).  As a result, many
common lifetime bugs become compiler errors.  Briefly, when you intact with
"safe" tasks like `async_closure` and friends:
  - Value types like `int` may be passed everywhere without restriction.
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

As of 2024, C++ does not allow `co_await` in `catch` blocks or destructors, so
there's no great way to guarantee async cleanup as the scope exits. This, in
turn, makes it extremely hard to [write exception-safe async
code](examples/CollectAllWindowedAsync.cpp). The pre-existing `co_scope_exit`
helps, but its API is clumsy and encourages memory-safety bugs.

`coro/safe` constructs like [`async_closure()`](AsyncClosure.md) and
`AsyncObject` provide a strong form of async RAII:
  - Most users will be happy with `SafeAsyncScope.h` for async cleanup. For
    library authors, the `co_cleanup(async_closure_private_t)` method is
    guranteed to be invoked, subject to minor caveats. Details in
    `CoCleanupAsyncRAII.md`.
  - Cleanup invocation is exception-safe, handling both errors in the "inner
    scope" and from sibling data with `co_cleanup`.
  - Both "closure body" and "cleanup" exceptions propagate to the caller.
    There is no channel for multiple errors, so one will be picked
    arbitrarily.  Currently (this is not a contract), any "inner" error
    takes priority over "cleanup" ones, since it's more likely to be the
    root cause.
  - Unlike synchronous RAII, cleanup of sibling objects is currently unordered,
    to potentially later support concurrent cleanup of unrelated objects.
  - The RAII implementation integrates with `SafeAlias.h` type annotations to
    ensure that `co_cleanup` calls cannot access the data belonging to siblings
    with `co_cleanup` -- those could have been cleaned up.
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

## Just show me some code!

Start with `folly/coro/safe/examples/CollectAllWindowedAsync.cpp`, which
demonstrates how a real-world scatter-gather utility becomes safe & legible
once migrated to `coro/safe` APIs.

The test `FooOnScope` in `AsyncScopeObjectTest.cpp` showcases a variety of
ways of scheduling background tasks from class member coros.

## But I need "Feature X"!

Please review `FutureWork.md`, and reach out to the author to discuss your idea.

## Why do you mix `underscore_separated` and `camelCase`?

This split-brain situation is a side effect of the `folly` team trying to move certain vocabulary types from the broadly-accepted `camelCase` to the more `std`-friendly `underscore_separated`. For example, `folly::coro` has both `blockingWait` (85% of callsites) and `blocking_wait` (intended future?).

As a result, identifiers in domains that were "more like camelcase libraries" stayed camelcase (`AsyncScope` -> `SafeAsyncScope`). While things that maybe looked more like `std` or core `folly::coro` vocabulary types became underscore-separated (i.e. `make_in_place`, `as_capture`, `async_closure`).

The original author is personally not a fan of this confusing state, and would welcome encouragement to either:
  - Standardize all APIs on the more prevalent `camelCase`, or
  - Introduce a *very* obvious guideline for which style applies when.
