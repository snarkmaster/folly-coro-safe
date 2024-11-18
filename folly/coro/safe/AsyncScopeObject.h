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

#include <folly/coro/safe/AsyncObject.h>

namespace folly::coro {

// If your `AsyncObject` includes one or more `SafeAsyncObject`-like
// `Slot`s, then you should `public`ly inherit from this class, and declare
// those members as `ScopeSlot<>` instead of `Slot<>`.
//
// This enables `.withObject(executor).scheduleSelfClosure(...)` support
// on those slots.
//
// Note: Conceptually, this should have been a mixin, but that would require
// a majort reshuffle the `Slot` implementation for little benefit.  It
// should be vanishingly rare that a derived class wants a `ScopeSlot`, and
// the base cannot be changed to derive from this.
class AsyncScopeSlotObject : public AsyncObject {
 private:
  template <typename, typename, auto>
  class ScopeContextWithObjectSlot;

 public:
  // Passed by `scheduleSelfClosure()` as the closure's first arg.
  // Its `AsyncObjectRefForSlot` specialization enforces the slot restriction.
  template <typename Obj, auto AllowedPtrToSlot>
  class SlotLimitedObjectPtr : public AsyncObjectPtr<Obj> {
   public:
    explicit SlotLimitedObjectPtr(detail::capture_private_t, auto refBind)
        : AsyncObjectPtr<Obj>(&std::move(refBind).what_to_bind()) {}
  };

 private:
  template <typename, auto>
  class ScopeSlotProxy;

  // Proxy returned by `withObject()` below.
  // Only provides `scheduleSelfClosure()`.
  // Use the regular scope API via `p->scopeSlot_(p)->with(executor)`.
  template <typename ScopeCtx, typename Obj, auto PtrToSlot>
  class ScopeContextWithObjectSlot : ::folly::NonCopyableNonMovable {
   public:
    // Passes a ref to the current object as the closure's first arg.
    auto scheduleSelfClosure(auto make_inner_coro, auto&&... as) && {
      // XXX explain safety
      // XXX same note on `restricted` as for scheduleScopeClosure
      // XXX lose or explain non_const
      auto [b] = folly::bindings::by_non_const_ref(obj_);
      return std::move(ctx_).schedule(async_closure(
          std::move(make_inner_coro),
          detail::async_closure_scope_self_ref_hack<
              SlotLimitedObjectPtr<Obj, PtrToSlot>,
              decltype(b)>{std::move(b)},
          std::forward<decltype(as)>(as)...));
    }

   private:
    template <typename, auto>
    friend class ScopeSlotProxy;

    explicit ScopeContextWithObjectSlot(Obj& obj, auto makeCtx)
        : obj_(obj), ctx_(makeCtx()) {}

    Obj& obj_;
    ScopeCtx ctx_;
  };

  template <typename Obj, auto PtrToSlot>
  class ScopeSlotProxy : public SlotProxy<Obj, PtrToSlot> {
   public:
    // `SafeAsyncScopeProxy::with()` with `scheduleSelfClosure()` support.
    //
    // NB: `FOLLY_NOINLINE` is required by `FOLLY_ASYNC_STACK_RETURN_ADDRESS`.
    template <size_t NumTokens = 0>
    FOLLY_NOINLINE auto withObject(
        Executor::KeepAlive<> executor,
        std::array<CancellationToken, NumTokens> tokensToMerge = {},
        source_location sourceLoc = source_location::current(),
        void* returnAddress = FOLLY_ASYNC_STACK_RETURN_ADDRESS()) && {
      auto makeCtx = [&]() {
        return this->slotCaptures()->with(
            std::move(executor),
            std::move(tokensToMerge),
            std::move(sourceLoc),
            returnAddress);
      };
      return ScopeContextWithObjectSlot<decltype(makeCtx()), Obj, PtrToSlot>{
          obj_, std::move(makeCtx)};
    }

   private:
    friend class AsyncObject;

    explicit ScopeSlotProxy(auto& obj, auto& slotCaptures)
        : SlotProxy<Obj, PtrToSlot>(obj, slotCaptures), obj_(obj) {}

    Obj& obj_;
  };

 protected:
  template <typename T, typename ContainedIn, auto PtrToPrevSlot>
  class ScopeSlot : public Slot<T, ContainedIn, PtrToPrevSlot> {
   public:
    using Slot<T, ContainedIn, PtrToPrevSlot>::Slot;

   private:
    friend class AsyncObject;

    template <typename O, auto S>
    using Proxy = ScopeSlotProxy<O, S>;
  };
};

namespace detail {
template <std::derived_from<AsyncObject> T, auto AllowedPtrToSlot>
struct AsyncObjectRefForSlot<
    AsyncScopeSlotObject::SlotLimitedObjectPtr<T, AllowedPtrToSlot>> {
  template <auto PtrToSlot>
  static constexpr decltype(auto) validateAndGetObjectRef(auto& objPtr) {
    static_assert(
        AllowedPtrToSlot == PtrToSlot,
        "When you use `scheduleSelfClosure`, the first argument in the "
        "resulting closure cannot be used to access `Slot`s besides "
        "`ScopeSlot` it was scheduled on -- those other slots may have "
        "already been cleaned up.");
    return *objPtr;
  }
};
} // namespace detail

/// Before using this, review the `AsyncObject.h` doc for high-level usage.
/// Crucially, coroutine functions in your class should typically be
///   - `static`, with the first arg a generic self-pointer: `auto me`
///   - Declared `NowTask` if always used via `async_closure` or
///     immediately awaited, `SafeOrNowTask` otherwise.
///
/// Inherit from this `public`ly to make your class an `AsyncObject` that
/// owns a single `SafeAsyncScope`.  See "Async cleanup" in docblock of
/// `AsyncObject.h` to add other async cleanup logic to your class.
///
///   class YourClass : public AsyncScopeObject { ... };
///   auto p = asyncObjectPtr<YourClass>(ctor, args);
///
/// If your sub-task doesn't need to schedule others, do this:
///   p->scope(p)->with(executor).schedule(async_closure(...));
///
/// To schedule recursively, use the following special API that enforces
/// referential safety.  The closure's first arg is an object pointer that
/// blocks access to all `Slot`s besides the one you scheduled on.  Note the
/// `.` before `withObject`.
///   p->scope(p).withObject(executor).scheduleSelfClosure(...);
template <typename CancelPolicy>
class AsyncScopeObject : public AsyncScopeSlotObject {
 private:
  ScopeSlot<
      SafeAsyncScope<CancelPolicy>,
      AsyncScopeObject<CancelPolicy>,
      noPriorSlot>
      scope_;

 protected:
  ~AsyncScopeObject() = default; // Meant to be a base class only
  explicit AsyncScopeObject(AsyncObjectTag tag)
      : scope_(std::move(tag), safeAsyncScope<CancelPolicy>()) {}

  // Schedule tasks via `co_await p->scope(p)->with(executor).schedule(...)`,
  // or use other `SafeAsyncScope` APIs (like `scheduleScopeClosure`).
  //
  // If your task needs access to this object, do this -- note the `.`:
  //   .withObject(executor)->scheduleSelfClosure(...);
  //
  // The child MAY expose this as `public` via a `using` declaration.
  [[nodiscard]] static decltype(auto) scope(auto& p) { return p->scope_(p); }

  friend class AsyncObject; // for `LastSlot`
  // Use `scope()` to schedule tasks.  Derived classes need this
  // pointer-to-member ONLY so they can declare successor slots.
  static constexpr auto LastSlot = &AsyncScopeObject::scope_;
};

} // namespace folly::coro

namespace folly::detail {
// Same story as `AsyncObjectPtr` -- details there.
template <typename T, auto P>
struct safe_alias_for_<
    ::folly::coro::AsyncScopeSlotObject::SlotLimitedObjectPtr<T, P>>
    : safe_alias_constant<safe_alias::shared_cleanup> {};
} // namespace folly::detail
