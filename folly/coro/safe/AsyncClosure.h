/*
 * Copyright (c) Meta Platforms, Inc. and affiliates.
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */

#pragma once

#include <folly/coro/safe/detail/AsyncClosure.h>

namespace folly::coro {

/*
XXX edit

See `SaferCoro.md` for why/how to use `async_closure()`.  In short, this
gives you exception-safe async cleanup for special args, and ensures that
the data those "cleanable" args can access will outlive the cleanup.

For safe cleanup, we wrap your task in an "outer" coro frame:
  - For any "special" argument types (see file docblock), places them into
    a `unique_ptr<tuple<...>>`.
  - Creates the "inner awaitable" by calling `makeInnerTask` with those
    special arguments replaced by their corresponding reference types, e.g.
    `asyncClosure{InPlace,Arg}` becomes `AsyncOuterClosurePtr`.
  - Regular arguments get forwarded to `makeInnerTask` (should be by-value).
  - Returns an outer `ValueTask`, which awaits your inner task, awaits
    cleanup, and only then destructs the special-arg tuple.

Control flow is what you'd expect from a regular coro:
  - All of the above "argument-binding" and "coro creation" work is eager.
  - Your inner task & subsequent cleanup are lazy-start -- the caller must
    await the returned `ValueTask`.
*/
///
/// ## How to implement `co_cleanup` -- async cleanup guarantees
///
/// Types passed `as_capture()` may implement one of these methods:
///
///   co_cleanup(async_closure_private_t)
///   co_cleanup(async_closure_private_t, const exception_wrapper*)
///
/// Use the latter variant to inspects the error from your inner coro, or
/// (should it throw) from your args' `setParentCancelToken()` callbacks.
///
/// Key things to know:
///
/// (0) See if you can avoid writing a `co_cleanup()` class.  In many cases,
///      `AsyncScopeObject` or `AsyncObject` will do what you need, while a
///      offering a `Slot` API that is harder to misuse.
///
/// (1) `co_cleanup` may return `Task<void>` or a tuple of such tasks.
///     Prefer tuples over composing unrelated cleanup tasks via `co_await`:
///       - Exception-safety: Allow the cleanups to fail independently.
///       - Can save a coro frame allocation.
///       - Potentially, enable concurrent cleanup in the future.
///     Example:
///       auto co_cleanup(async_closure_private_t, const exception_wrapper* e) {
///         return std::tuple{
///             another_cleanup(e),
///             // Capture a copy of the **pointer** -- it's populated later.
///             [=]() -> Task<void> { ... }()};
///       }
///
/// (2) For any `co_cleanup` object, it must be safe for it to be destructed
///     WITHOUT awaiting cleanup IF the closure has not yet reached initial
///     suspend.  In other words, closure setup MAY fail before the
///     `setParentCancelToken` phase -- for example:
///        - Closure argument construction may throw.
///        - Closure or `co_cleanup` coro allocation may get `bad_alloc`.
///        - Eager setup code in user-supplied `co_cleanup` may fail.
///     The simplest form of this guarantee is:
///        - Your object does not need cleanup when just-constructed.
///        - It is `NonCopyableNonMovable`, and created via
///          `as_capture(make_in_place<YourClass>(...))`, so that the user
///          is UNABLE to accidentally put it in a state requiring cleanup
///          before the closure starts.
///
///     NOTE: There is NO great alternative to this design.  Yes, it would
///     be possible to reshuffle most closure setup to happen BEFORE your
///     cleanup-capable object is constructed -- including, even, making
///     `co_cleanup` a static method that preallocates the cleanup coro with
///     the future address of your object.  However, nothing can work around
///     the fundamental issue, which is that if your object's constructor
///     throws, it could be in a state requiring **partial** async cleanup
///     (e.g. of base classes), and this is something that simply can't be
///     handled properly without "async destructor" support in the language.
///
/// (3) The `co_cleanup()` function runs EAGERLY, when the `async_closure()`
///     is being created. Therefore:
///       - All your cleanup logic MUST sit in a lazy-start coroutine.
///       - If you need to inspect the error, you must copy the
///         `exception_wrapper` pointer into your cleanup, NOT the value.
///       - For `bad_alloc` safety, it may be a good idea to eagerly
///         pre-allocate any coroutines that your cleanup needs to
///         `co_await`, and to move them into your main cleanup task.
///         Otherwise, your cleanup may fail partially due to OOM.
///
/// (4) Cleanup should try to AVOID THROWING, but it's not fatal if it does.
///     If your inner coro (or `setParentCancelToken`) throws, that error
///     will (for now -- subject to change) mask any `co_cleanup`
///     exceptions.  If some `co_cleanup`s throw, cleanup will still be
///     attempted for the other args.  When the inner coro succeeds, but any
///     cleanups fail, the closure exits with one of the cleanup errors.
constexpr auto async_closure(auto make_inner_coro, auto&&... args) {
  return detail::bind_captures_to_closure</*force outer coro*/ false>(
             std::move(make_inner_coro), std::forward<decltype(args)>(args)...)
      .release_outer_coro();
}

/// Some APIs are designed to be exclusively, or mostly, used via
/// `async_closure()`. This lets you declare one statically, e.g.
///
///   static constexpr auto ac_foo = as_async_closure(
///     // wrap single-overload function
///     closureTaskFn
///     // OR wrap a generic function (static OK via `Class::name`)
///     FOLLY_INVOKE_QUAL(genericClosureTaskFn)
///     // OR wrap a lambda
///     [](auto x) -> ClosureTask<void> { co_await moo(x + 1); }
///   );
///
/// The `ac_` name prefix is highly encouraged, since it signals to the
/// caller that this function follows the `async_closure()` calling
/// convention (`folly::bindings` support, `as_capture` support), and has
/// safer semantics.
///
/// NB: When e.g.  `ac_` methods are passed into `schedule*Closure` APIs,
/// this nests `async_closure` twice, but there should be no runtime cost,
/// even in the presenece of `as_capture`s.  That's because the inner
/// `async_closure` would get only refs, and thus be able to elide its outer
/// closure, and storage.
template <detail::is_stateless_class_or_func Fn>
constexpr auto as_async_closure(Fn) {
  return [](auto&&... args) {
    return async_closure(Fn{}, std::forward<decltype(args)>(args)...);
  };
}

/// A variant of `async_closure() to help reduce heap allocations when
/// passing many in-place args.  Currently (this may change!), if none of an
/// `async_closure`'s arguments provide `co_cleanup(async_closure_private_t)`,
/// it will pass each individual `make_in_place` arg on-heap.  If you're in
/// an edge case where "allocating an outer coro + unique_ptr<tuple<...async
/// args...>" sounds cheaper than "allocating many in-place args on-heap",
/// try passing this argument.  If it's needed often, we may provide this
/// behavior as a heuristic.
constexpr auto async_closure_force_outer_coro(
    auto make_inner_coro, auto&&... args) {
  return detail::bind_captures_to_closure</*force outer coro*/ true>(
             std::move(make_inner_coro), std::forward<decltype(args)>(args)...)
      .release_outer_coro();
}

} // namespace folly::coro
