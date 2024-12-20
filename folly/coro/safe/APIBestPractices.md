## Audience & purpose

You already understand the core premise of [structured concurrency with
coroutines]( https://ericniebler.com/2020/11/08/structured-concurrency/),
and why the safest default is either to:
  - Await your coros in the same expression that created them (`NowTask`), or
  - Pass in *only* value arguments (`ValueTask`).

For handling less-structured situations (like deferred & background tasks):
  - You're familiar with the toolkit overview in `SaferCoro.md`.
  - You know the safety benefits of [`async_closure()`](AsyncClosure.md), and,
    optionally, `AsyncObject`.
  - You understand the purpose of `capture`s and `AsyncObjectPtr`s, and how they
    leverage the hierarchy of `SafeAlias.h` markings to determine whether or not
    you can pass references into child closures, or schedule them on
    `SafeAsyncScope`s.

This document will show you idiomatic patterns for safe coro APIs, and
explain the reasons behind them.  You will learn how to:

  - Write non-static member APIs with `MemberTask<Ret> yourFn(...)`.

  - Create `ac_`-prefixed callables to wrap async-closure-required coros for
    better clarity and usability.

  - How to use `FOLLY_INVOKE_*` macros to pass generic functions into closure APIs.

## Just show me example code!

Follow along with the `FooOnScope` test in `AsyncScopeObjectTest.cpp`.  That
test showcases most of the patterns discussed below.

## Which safe coro type do I use?

The first rule of `folly/coro/safe` is that you do **not** write vanilla
`Task<>`s.  Instead, you will use `NowTask`, or one of a number of type aliases
from `SafeTask.h`. Pick the type via this checklist.

First, let's handle the common cases:
  - Coro only takes value-semantic types: `ValueTask<T>`.
  - Coro awaited immediately upon creation: `NowTask<T>`.
  - Non-static member coro of a nonempty class -- `MemberTask<T>`.  It is
    immediately-awaitable / non-movable, and so is equivalent to `NowTask`
    for non-closure usage.  The upside is that callers can invoke it via
    `async_closure()`, and get a movable task with stronger safety marking.
    A special type is required here since the implicit object parameter
    is a reference, and we know nothing about its lifetime safety.

### When to use `async_closure`

For more complex usage, check if your coro benefits from `async_closure()`
semantics.  Reasons to use it:
  - Some of the coro's arguments need `co_cleanup()` aka async RAII.
    For example, you might be passing `safeAsyncScope<>()` to your coro.
  - You want to use `capture` ref wrappers to safely pass objects by-reference
    to child coros that are not `NowTask`.
  - You benefit from `async_closure()` auto-upgrading `after_cleanup_capture<>`s
    to plain `capture<>`s (absent any `shared_cleanup` arguments). For example,
    this can help nest work with multiple `SafeAsyncScope`s.
  - *(weak)* You want easy syntax for in-place construction of coro args.
  - *(weak)* You want the slightly safer, but less standard, argument binding
    semantics of `folly/lang/Bindings.h`.
  - *(weak/uncommon)* The coro is generic in its arguments, and needs the
    safety of the resulting `SafeTask` to vary accordingly, but using
    `AutoSafeTask` is too onerous.
  - *(rare)* A coro's arg uses the `setParentCancelToken()` protocol.

If you require `async_closure()`:
  - The inner coro will almost always be `ClosureTask` (free or static
    function) or `MemberTask` (non-static member).
  - To save the users from typing `async_closure()` at each callsite,
    consider wrapping your API with an `ac_` prefix.  The prefix tells the
    user to expect `capture` semantics and `folly::bindings` modifiers:
    ```cpp
    ClosureTask<int> someFn();
    inline constexpr auto ac_someFn = as_async_closure(
        FOLLY_INVOKE_QUAL(someFn));  // for static functions, too

    struct Foo {
      MemberTask<void> anotherFn();
      static constexpr auto ac_anotherFn = as_async_closure(
          FOLLY_INVOKE_MEMBER(anotherFn));
    };
    ```
  - You may use `ac_`-prefixed function with `schedule*Closure()` without
    runtime cost, so consider hiding the non-`ac_` body.  Options include:
    `private:`, `Impl` suffix, `detail` namespace, or an inline lambda:
    ```cpp
    inline constexpr auto ac_fivePlus = as_async_closure(
        [](auto intCap) -> ClosureTask<int> {
          co_return 5 + *intCap;
        });
    assert(15 == co_await ac_fivePlus(10);
    ```

### Non-`async_closure` safe tasks

If your complex API does not need `async_closure()`, here are some more options:
  - `CoCleanupSafeTask` -- for free or static coros meant to be scheduled on a
    `SafeAsyncScope`, or moved into another `co_cleanup()` type.
  - `AutoSafeTask<T, ArgTypes...>` -- for power users writing a generic coro
    that must be a `SafeTask` (e.g.  to be optionally backgrounded, or to be
    moved into a child).  Note that `ArgType`s are only used to determine
    the safety level, and are normally a subset of your coro's argument
    types.  If an arg's type is unsafe, this alias falls back to `NowTask`, but it
    **cannot** fall back to `NowTask` if the coro's callable is stateful (i.e.
    lambda with captures, non-static member of a non-empty class), and
    compilation will fail with a "Bad SafeTask" error.

### Recursive scheduling

When scheduling a coro on a `SafeAsyncScope` -- whether using
`CoCleanupSafeTask` or an `async_closure`, you can allow that coro to
schedule additional coros on the same scope.  The steps are simple:
  - Have the child coro take `co_cleanup_capture<Obj& OR Scope&>` or
    `AsyncObjectPtr<Obj>` as the first parameter -- typically, the signature
    will be `auto`.
  - Invoke the child via
    - `scheduleSelfClosure()` if the scope is part of
    an `AsyncObject`), or
    - `scheduleScopeClosure()` if the scope is a `co_cleanup_capture` arg of a
    closure.

```cpp
struct Recurse : public AsyncScopeObject<CancelViaParent> {
  Recurse(AsyncObjectTag t)
      : AsyncScopeObject<CancelViaParent>(std::move(t)) {}

  using AsyncScopeObject<CancelViaParent>::scope;

  static CoCleanupSafeTask<void> recurse(Me me, int n) {
    std::cout << "to recurse: " << n << std::endl;
    if (n) {
      co_await me->scope(me)
          .withObject(co_await co_current_executor)
          .scheduleSelfClosure(Recurse::recurse, n - 1);
    }
  }
};
```

## What's with the `FOLLY_INVOKE_*` macros in the examples above?

Coro functions may have multiple overloads -- for example, it is common
for `AsyncObject` coros to be `static` and take a generic `me` pointer
type.

Unfortunately, C++ lacks a type for overload sets, and cannot pass them as
function or parameter parameters (see [P3312R0](https://wg21.link/p3312r0)
or [P3360R0](https://wg21.link/p3360r0) for a 2024-vintage discussion).
Thus, users cannot directly pass polymorphic functions into
`async_closure()`, `as_async_closure()`, `scheduleSelfClosure()`, etc.

Callable objects (like the inline lambda example above) avoid this issue.
You can also define an explicitly named callable object, and put the
implementation in a `.cpp` file.

However, if you prefer functions, `folly/functional/Invoke.h` provides two
macros to wrap an expression in an equivalent callable object, both
implemented along these lines

```cpp
[](auto&&... a) -> decltype(auto) {
  return YOUR_FN(std::forward<decltype(a)>(a)...);
}
```

`FOLLY_INVOKE_MEMBER` wraps non-static member functions, while
`FOLLY_INVOKE_QUAL` wraps qualified & unqualified free & static functions.
