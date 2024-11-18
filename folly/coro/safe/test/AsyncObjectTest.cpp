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
#include <folly/coro/safe/AsyncObject.h>

namespace folly::coro {

struct Empty : public AsyncObject {
  explicit Empty(AsyncObjectTag) {}
};

CO_TEST(AsyncObject, emptyOnScope) {
  co_await async_closure(
      [](auto scope) -> ClosureTask<void> {
        auto e =
            asyncObjectPtr<Empty>(scope->with(co_await co_current_executor));
      },
      safeAsyncScope<CancelViaParent>());
}

CO_TEST(AsyncObject, emptyOnClosure) {
  co_await async_closure(
      [](auto obj) -> ClosureTask<void> {
        // NB: For the time being, we do NOT have any special logic to omit
        // the outer coro frame for "no-cleanup" `AsyncObject` args,
        // although it would not be hard to add -- just make the
        // `privateHackSetParentCancelToken` conditional on any of the slots,
        // or the main object, having `co_cleanup`.
        static_assert(
            std::is_same_v<decltype(obj), co_cleanup_capture<Empty&>>);
        co_return;
      },
      asyncObjectAsArg<Empty>());
}

struct HasCleanup {
  explicit HasCleanup(auto unsafeRef) : didCleanupPtr_(&unsafeRef.get()) {}
  bool* didCleanupPtr_;
  folly::coro::Task<void> co_cleanup(async_closure_private_t) {
    *didCleanupPtr_ = true;
    co_return;
  }
};

// XXX test this alone

struct OneSlot : AsyncObject {
  explicit OneSlot(AsyncObjectTag priv, auto a) : first_(std::move(priv), a) {}

  Slot<HasCleanup, OneSlot, noPriorSlot> first_;

  static constexpr auto LastSlot = &OneSlot::first_;
};

struct TwoSlots : OneSlot {
  explicit TwoSlots(AsyncObjectTag priv, auto a, auto b)
      : OneSlot(std::move(priv), a), second_(first_.nextTag(), b) {}

  Slot<HasCleanup, TwoSlots, &OneSlot::first_> second_;

  static constexpr auto LastSlot = &TwoSlots::second_;
};

CO_TEST(AsyncObject, twoSlots) {
  bool cleanedUp1 = false, cleanedUp2 = false;
  co_await async_closure(
      [](auto scope, auto clean1, auto clean2) -> ClosureTask<void> {
        auto p = asyncObjectPtr<TwoSlots>(
            scope->with(co_await co_current_executor), clean1, clean2);
        EXPECT_FALSE(*p->first_(p)->didCleanupPtr_);
        EXPECT_FALSE(*p->second_(p)->didCleanupPtr_);
      },
      safeAsyncScope<CancelViaParent>(),
      manual_safe_ref(cleanedUp1),
      manual_safe_ref(cleanedUp2));
  EXPECT_TRUE(cleanedUp1);
  EXPECT_TRUE(cleanedUp2);
}

// XXX 3 slots also so we consume the SlotAfter sigil

// XXX explain the tradeoff between this and the slot API
#if 0
struct ManualObject : AsyncObject {
  std::tuple<Task<void>> co_cleanup(async_closure_private_t p) {
    return std::tuple{a.co_cleanup(p), b.co_cleanup(p)};
  }

  // XXX this kind of thing would make scopes with different reference kinds
  // work.  The downside of course is that you can forget to do cleanup.
  //
  // SafeAsyncScope<CancelViaParent> scope;
  //
  // scope->with(executor).schedule() -- sugar for
  // scope.ref().with(executor).schedule(task)
  // scope.restrictedRef().with(executor).schedule(task)

  HasCleanup a;
  HasCleanup b;
};

struct ChildManualObject : ManualObject {
  std::tuple<Task<void>> co_cleanup(async_closure_private_t p) {
    return std::tuple_cat(
        ManualObject::co_cleanup(p),
        std::tuple{
          c.co_cleanup(p),
          [this]() -> Task<void> {
            // do other cleanup stuff
            co_return;          
          }()
        });
  }
  
  HasCleanup c;
};
#endif

struct ObjNeedsCancelToken : public AsyncObject {
  explicit ObjNeedsCancelToken(AsyncObjectTag, auto ctokUnsafeRef)
      : cancelTokenPtr_(&ctokUnsafeRef.get()) {}
  CancellationToken* cancelTokenPtr_;
  void setParentCancelToken(
      async_closure_private_t, const CancellationToken& t) {
    *cancelTokenPtr_ = t;
  }
};

// It is important that the cancellation token be set synchronously, as soon
// as the `AsyncObject` is created -- otherwise, any synchronously-created
// descendants of the object could not be correctly connected to its token.
CO_TEST(AsyncObject, creatingObjectPtrSetsCancelToken) {
  // Should outlive the `SafeAsyncScope` since `ObjNeedsCancelToken` takes a
  // raw pointer to `ctok` and is managed by that scope.
  CancellationToken objCtok;
  // The `EXPECT_TRUE` is only meaningful if the coro has a nonempty token.
  CancellationSource cancelSource;
  co_await co_withCancellation(
      cancelSource.getToken(),
      async_closure(
          [](auto scope, auto objCtokRef, auto expectedCtok)
              -> ClosureTask<void> {
            EXPECT_FALSE(objCtokRef == expectedCtok);
            auto p = asyncObjectPtr<ObjNeedsCancelToken>(
                scope->with(co_await co_current_executor), objCtokRef);
            // Verify that `setParentCancelToken` was called on the object,
            // and that it received the outer closure's token.  This checks
            // token equality because the implementation calls `merge()`
            // with the single argument of `scope->parentCancelToken_` --
            // and that just copies the token.
            EXPECT_TRUE(objCtokRef == expectedCtok);
            EXPECT_TRUE(objCtokRef == co_await co_current_cancellation_token);
          },
          safeAsyncScope<CancelViaParent>(),
          manual_safe_ref(objCtok),
          cancelSource.getToken()));
}
// XXX repeat this test for an on-closure object!  the creation &
// token-provision paths are totally different

// XXX test passing new `AsyncObject` as an capture into a closure

// XXX test moving `AsyncObjectPtr` as an capture into a closure

// XXX test passing capture refs into `AsyncObject`

// XXX test `setParentCancelToken` delegation, and cancellation in general

// XXX verify that we clean up slots on an `AsyncObject` nested inside the
// slot of another

// XXX test that moving an AsyncObjectPtr into a sub-closure, and triggers
// shared_cleanup downgrades

} // namespace folly::coro

/* XXX best option for avoiding `Slot<>` repetition, CRTP is not helpful
class SomethingDerived {
  template <typename T, auto Ptr>
  using MySlot = Slot<T, SomethingDerived, Ptr>;
}
*/

// XXX async_closure test that shows it binds eagerly
