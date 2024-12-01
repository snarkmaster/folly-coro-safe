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

#ifndef _WIN32 // Explained in SafeTask.h

namespace folly::coro {

/// This is a brief summary.  Before writing critical code, please also get
/// familiar with `SaferCoro.md` and `AsyncClosure.md`.  If you require
/// custom async RAII, you must also carefully read `CoCleanupAsyncRAII.md`.
///
/*
XXX edit, mention outer coro elision, mention safetask-only + future extension

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
/// Future: It may be useful to support `NowTask` closures, to let users
/// write "structured concurrency"-style code with regular C++ by-reference
/// args, while taking advantage of `async_closure()`'s guaranteed
/// `co_cleanup()`.  Two UX options to consider:
/// (1) If `make_inner_coro` returns `NowTask`, the closure is `NowTask`.
/// (2) There's a separate API, like `async_now_closure()`.
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

#endif
