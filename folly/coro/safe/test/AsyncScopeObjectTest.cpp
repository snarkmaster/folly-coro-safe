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

#include <folly/coro/GtestHelpers.h>
#include <folly/coro/safe/AfterCleanup.h>
#include <folly/coro/safe/AsyncScopeObject.h>

using namespace folly::bindings;

namespace folly::coro {

class Foo : public AsyncScopeObject<CancelViaParent> {
 private:
  int a_;
  int b_;
  capture<std::atomic_int&> sum_;

 public:
  Foo(AsyncObjectTag t, int a, int b, capture<std::atomic_int&> sum)
      : AsyncScopeObject<CancelViaParent>(std::move(t)),
        a_(a),
        b_(b),
        sum_(sum) {}

  using AsyncScopeObject<CancelViaParent>::scope;

  // `AutoSafeTask` lets this be used without an `async_closure` wrapper.
  // A `static ClosureTask` coro would be usable without `async_closure`.
  template <typename Me>
  static AutoSafeTask<void, Me> addToSumStatic(Me me, int n) {
    // Shows that `MemberTask` is usable without `async_closure` too.
    co_await me->addToSum(n);
  }

  // `ac_` is the suggested naming convention for `as_async_closure` APIs.
  static constexpr auto ac_addToSumStatic =
      as_async_closure(FOLLY_INVOKE_QUAL(Foo::addToSumStatic));

  // One can also declare `ac_` APIs with all the code inline:
  static constexpr auto ac_addABToSum =
      as_async_closure([](auto me) -> ClosureTask<void> {
        me->sum_->fetch_add(me->a_);
        // NOTE: Since static functions take an explicit `me` pointer, we
        // can recursively schedule additional tasks on the same scope.
        me->scope(me)
            .withObject(co_await co_current_executor)
            .scheduleSelfClosure(ac_addToSumStatic, me->b_);
        co_return;
      });

  // Can be awaited immediately as-if a `NowTask` (in `addToSumStatic`), or
  // wrapped in an `async_closure()` via `FOLLY_INVOKE_MEMBER` (below).
  // Unlike the `static` functions, plain members cannot recursively schedule
  // more tasks on the scope, since we don't know the safety of `this`.
  MemberTask<void> addToSum(auto n) {
    sum_->fetch_add(n);
    co_return;
  }
};

CO_TEST(AsyncScopeObject, FooOnScope) {
  int sum = co_await async_closure(
      [](auto scope,
         auto sum) -> ClosureTask<move_after_cleanup<std::atomic_int, int>> {
        auto* exec = co_await co_current_executor;
        auto foo = asyncObjectPtr<Foo>(scope->with(exec), 100, 30, sum);
        // NB: `foo->scope(foo)` is also acceptable style, since for custom
        // `Slot` members, you need to resolve the member BEFORE passing it
        // the specific lifetime-safe pointer.  This will usually require
        // (absent a `static` getter like this one) mentioning the pointer
        // twice.  Any single-pointer alternative would be unwieldy, e.g.
        //   AsyncObject::getSlot(ptr, MyObj::mySlot_)
        Foo::scope(foo).withObject(exec).scheduleSelfClosure(
            FOLLY_INVOKE_QUAL(Foo::addToSumStatic), 1);
        Foo::scope(foo).withObject(exec).scheduleSelfClosure(
            FOLLY_INVOKE_MEMBER(addToSum), 2);
        Foo::scope(foo).withObject(exec).scheduleSelfClosure(
            Foo::ac_addToSumStatic, 4);
        Foo::scope(foo).withObject(exec).scheduleSelfClosure(
            Foo::ac_addABToSum);
        // We can't schedule something referencing `foo` on a scope because
        // there's no `async_closure()`-safe way to take a reference to
        // `foo` in the closure.  Even if we moved `foo` into the closure,
        // the result still has non-schedulable `shared_cleanup` safety.
        Foo::scope(foo)->with(exec).schedule(async_closure(
            [](auto sum) -> ClosureTask<void> {
              sum->fetch_add(200);
              co_return;
            },
            sum));
        co_return move_after_cleanup<std::atomic_int, int>(sum);
      },
      safeAsyncScope<CancelViaParent>(),
      as_capture(make_in_place<std::atomic_int>(1000)));
  EXPECT_EQ(1337, sum);
}

#if 0 // XXX maybe genericize?
CO_TEST(AsyncScopeObject, FooOnClosure) {
  co_await async_closure(
      [](auto obj) -> ClosureTask<void> {
        // NB: For the time being, we do NOT have any special logic to omit
        // the outer coro frame for "no-cleanup" `AsyncObject` args,
        // although it would not be hard to add -- just make the
        // `privateHackSetParentCancelToken` conditional on any of the
        // slots, or the main object, having `co_cleanup`.
        static_assert(std::is_same_v<decltype(obj), co_cleanup_capture<Empty&>>);
        co_return;
      },
      asyncObjectAsArg<Empty>());
}
#endif
// XXX test simple AsyncScopeObject
// XXX test AsyncScopeObject + extra 1, 2 slots
// XXX test AsyncScopeObject scheduleSelfClosure
// XXX test body_only_ref_ downgrade

} // namespace folly::coro
