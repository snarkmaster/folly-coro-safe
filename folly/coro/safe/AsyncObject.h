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

#include <folly/coro/Baton.h>
#include <folly/coro/safe/AsyncClosure.h>
#include <folly/coro/safe/SafeAsyncScope.h>
namespace folly::coro {

class AsyncObject;

template <std::derived_from<AsyncObject>>
class AsyncObjectPtr;

// Pass an `AsyncObject` as an `capture` into an `async_closure()`.
template <std::derived_from<AsyncObject>>
auto asyncObjectAsArg(auto&&...);

// Make an `AsyncObject`, returns a pointer whose dtor triggers async cleanup.
template <std::derived_from<AsyncObject> T>
AsyncObjectPtr<T> asyncObjectPtr(
    detail::is_SafeAsyncScopeContextProxy auto, auto&&...);

class AsyncObjectTag;

namespace detail {
// Slot tags (call `nextTag()` on the previous slot) directly enforce that
// "init order == declaration link-up order".  This is not (yet)
// super-useful, since cleanup ordering is **unspecified** (to allow future
// parallelism), but the UX cost of `.nextTag()` is very low -- each slot's
// ctor must take a private tag anyway to enforce the core invariant that
// "anytime an `capture` is created, it is safely managed".  We let the
// standard linter enforce "declaration order matches init order".
template <typename C, size_t>
class AsyncObjectSlotTag;
} // namespace detail

/// XXX explain asyncObjectPtr vs asyncObjectAsArg user scenarios

/// If you just need your class to own a `SafeAsyncScope` for background
/// tasks, review the high-level docs here, and use `AsyncScopeObject.h`.
///
/// `AsyncObject` guarantees that async cleanup is awaited before your
/// object is destroyed.  The emphasis is on composing smaller "async
/// cleanup" pieces into higher-level classes.
///
/// It addresses two related "async RAII" scenarios:
///
/// (1) Normal C++ scoping / RAII rules: Destroying the object's
///     `AsyncObjectPtr` will first triggers async cleanup, then destroys
///     the object.  A pre-specified "destructor scope" awaits the cleanup.
///
///     Edge case: If you make an `AsyncObject` that doesn't require any
///     cleanup, its destructor is still deferred to the outer scope.
///     However, it is _usually_ a bad idea to delay freeing resources.
///
/// (2) Using `AsyncObject` as an `async_closure()` argument. The closure
///     provides its usual safe cleanup & lifetime semantics, and the object
///     is essentially "just another `as_capture()` needing cleanup".
///     XXX either triggers "shared_cleanup" downgrades, or is a restricted ref.
///
///     XXX
///
/// XXX MAIN CONCERN 1: sigh in (2) we need some shim to iterate over slots
/// when the `AsyncObject` is just one of many args.
///
/// ## Creating an `AsyncObject`
///
/// To be usable with `asyncObjectPtr<T>()`, your class `T` must:
///   - Derive from `AsyncObject` (`public` inheritance).
///   - Implement an `AsyncObjectTag` constructor.
///   - For async cleanup, see "Async cleanup" below.
///
/// For the scenarios above:
/// (1) Use `auto p = asyncObjectPtr<YourClass>()`.  Destroying the pointer
///     starts async cleanup of the object on its parent async scope.
/// (2) XXX
///
/// There is no "async constructor", because that can be trivially modeled by
///   auto co_makeFooPtr() -> Task<...> { co_return asyncObjectPtr<Foo>(); }
///
/// # Memory safety enforcement
///
/// `AsyncObject`s use the same `SafeAlias.h`-based memory-safety design as
/// `async_closure()`, with one caveat.  Since object cleanup is always
/// deferred to a `SafeAsyncScope`, all data passed into `AsyncObject`s must
/// have safety `>= co_cleanup_safe_ref`.  This is enforced for constructor
/// arguments, but you should practice the same discipline for any setters.
///
/// Future / XXX: Add a generic enforcement concept to `SafeAlias`, suggest
/// it above.
///
/// As elsewhere, a less-safe reference passed into your object would not
/// necessarily be alive at the time of your object's async cleanup.
///
/// ## Async cleanup
///
/// You can implement a combination of these patterns, from best to worst:
///
///   - Derive from `AsyncScopeObject` and use `scope()->` methods.
///
///   - Place `co_cleanup` components into a (compile-time) linked list of
///     `Slot`s, declared thus:
///
///       Slot<CleanupComponent1, YourClass, noPriorSlot> a;
///       Slot<CleanupComponent2, YourClass, &YourClass::a> b;
///       static constexpr auto LastSlot = &YourClass::b;
///
///     Class initializers instantiate the same list via single-use tags:
///
///       YourClass(AsyncObjectTag tag)
///           : a_(std::move(tag)), b_(a_.nextTag()) {}
///
///     The point of `Slot`s is to let `AsyncObject` see all your
///     `co_cleanup()` members.
///
///     WATCH OUT: Although `Slot`s are strictly ordered, cleanup order is
///     currently **unspecified**.  If we were going to order cleanup,
///     "reverse of slot order" would be the logical choice, matching
///     destruction order.  However, there's a strong case to be made for
///     making cleanup concurrent via `collectAll()` or similar -- also see
///     `detail/AsyncClosure.h`.
///
///   - Implement `co_cleanup(async_closure_private_t)` for your specific
///     object.  This is tricky because anything can throw, but in reliable
///     programs, cleanup must tackle partial failure.  Even though you can
///     do the following instead of `Slot`s, it is DISCOURAGED, because it
///     discards the guarantees of `Slot`s:
///       * You can't forget cleanup.
///       * XXX exception safety is harder, unless you JUST compose tuples
///     XXX show example
///       * XXX not accessing co_cleanup members during cleanup
///
/// XXX how does the above interact with the "merge" point between
///     slots and `co_cleanup`
///
/// ## Async cleanup & exception-safety
///
/// For a deep dive into `co_cleanup` exception-safety, read the docblock
/// in `AsyncClosure.h`. In short:
///
/// NONE of your object's async cleanup will be awaited if:
///   - Any of the "cleanup coro" plumbing throws (e.g. `bad_alloc`).
///   - Your constructor throws.
///
/// To mitigate this risk, your object is REQUIRED to either:
///   - [PREFERRED] Have a design making it safe to skip awaiting all cleanup
///     when the object ctor was invoked, its `co_cleanup` coro was
///     (potentially) allocated, but the object is otherwise unused.
///   - [LAST RESORT] If acceptable for your application, fatal in the
///     destructor when cleanup was skipped. Document this wart!
///
/// ## Writing coro "methods" on `AsyncObject`s
///
/// XXX both are shared_cleanup, so the point in (2) is only relevant once
/// we support non-cleanup `AsyncObject`
///
/// XXX state the above, and document self-scheduling, and it's probably all
/// we need.
///
/// In scenario (1), coros derived from the object are UNSAFE for background
/// scheduling, since it's very bug-prone to reason about whether a variable
/// outlives when the background scope is joined.  Best practice is to put
/// your data on an `async_closure`, see (2).
///
/// In scenario (2), the first argument of your static method will be
/// `co_cleanup_capture()
//
/// XXX unless you elect a restricted ref, the safety of these methods will
/// be at most `shared_cleanup` or `body_only_ref` because the refs safety
/// isn't backgroundable
///
///  => XXX fold in the  decent writing from the .md here (shared_ptr,
///  unique_ptr, etc)
///
/// For running class coros on scopes owned by the class, add this two-step
/// (for line # etc), `lifetimebound` ref instance method:
///   p->withScope(&Class::scope, executor)
///     .scheduleMemberClosure(&Class::coro, args...);
///
/// Design notes:
///   - Cannot schedule on object's dtor scope -- cleanup is unordered.
///   - Could theoretically safely schedule on outer-to-my-dtor-scope
///     scopes, BUT this would require runtime checks, which is a feature
///     that should be provided via `Captures.h` for all cleanup args.
///
/// XXX finish folding in AsyncObject.md
///
/// ## Why doesn't this use `folly::bindings` like `async_closure()`
///
/// Closures require binding magic for `as_capture()`, and are typically
/// used with lambdas taking `auto` args.  For objects, bindings are not
/// essential, and e.g. `make_in_place` support would complicate
/// constructor signatures (which are typically explicit and in header
/// files).  But, this feature is easy to add:
///
///   template <typename T, typename... Bs>
///   AsyncObjectPtr<T> asyncObjectPtrBindings(auto scopeCtx, Bs&&... bs) {
///     return std::apply(
///       [&](auto... bindings) {
///         return asyncObjectPtr(std::move(scopeCtx), std::move(bindings)...);
///       },
///       folly::bindings::ensure_binding_tuple(std::forward<Bs>(bs)...));
///   }
class AsyncObject : NonCopyableNonMovable {
 protected: // For `AsyncScopeSlotObject`
  // Returned by `mySlot_(ptr)` accessors.
  template <typename Obj, auto PtrToSlot>
  class SlotProxy : NonCopyableNonMovable {
   public:
    // Forward all actions to the underlying `co_cleanup_capture`.
    [[nodiscard]] constexpr decltype(auto) operator*() noexcept {
      return slot_.operator*();
    }
    [[nodiscard]] constexpr decltype(auto) operator->() noexcept {
      return slot_.operator->();
    }
    [[nodiscard]] constexpr decltype(auto) operator*() const noexcept {
      return std::as_const(slot_).operator*();
    }
    [[nodiscard]] constexpr decltype(auto) operator->() const noexcept {
      return std::as_const(slot_).operator->();
    }

   private:
    friend class AsyncObject;

    using SlotCaptures = decltype(member_pointer_traits<
                                  decltype(PtrToSlot)>::member_type::slot_);
    SlotCaptures& slot_;

   protected:
    // NB: `ScopeSlotProxy` needs the `Obj&` reference.
    explicit SlotProxy(Obj&, SlotCaptures& t) : slot_(t) {}
    decltype(auto) slotCaptures() { return slot_; }
  };

 private:
  // Implements `mySlot_(ptr)` accessors.
  //
  // Future: It may be reasonable to make this `protected` or even `public`,
  // but in that case we should enforce that `S` is a `Slot`.
  template <auto PtrToSlot, typename Ptr>
  static auto slotPtr(Ptr& ptr) {
    // Pointer-specific assertions happen in specializations of this func,
    // so it needs to know the slot (e.g. for `SlotLimitedObjectPtr`).
    auto& obj = detail::AsyncObjectRefForSlot<std::remove_cv_t<Ptr>>::
        template validateAndGetObjectRef<PtrToSlot>(ptr);
    auto& slot = (obj.*PtrToSlot).slot_;
    return typename member_pointer_traits<decltype(PtrToSlot)>::member_type::
        template Proxy<std::remove_reference_t<decltype(obj)>, PtrToSlot>{
            obj, slot};
  }

  // See `SlotProxy`, obtained via `yourSlot_(ptr)`, for the public accessors.
  //
  // Future: This could be relaxed to store non-cleanup types if there's a
  // legit use-case for e.g.  `setParentCancelToken` being invoked.  Note that a
  // "complete" implementation of non-cleanup slots is involved:
  //  - An `AsyncObject` without cleanup slots should be treated as a
  //    non-cleanup closure arg.
  //  - `slot()` retrieval should always allow access to non-cleanup slots.
  //  - The `safe_alias_for_` markings for non-cleanup `AsyncObject*` types
  //    would be different (i.e. `shared_cleanup` -> `body_only_ref`)
  //  - Etc.
  //
  // Future: Similarly, we could support `RestrictedSlot`s, which would ONLY
  // be able to issue restricted refs, improving the object's `safe_alias_of`.
  template <detail::has_async_closure_co_cleanup T>
  class SlotBase : NonCopyableNonMovable {
   protected:
    ~SlotBase() = default;
    explicit SlotBase(AsyncObjectTag, auto&&);

   private:
    friend class AsyncObject;

    template <typename O, auto P>
    using Proxy = SlotProxy<O, P>;

    co_cleanup_capture<T> slot_;
  };

  template <
      typename ContainedIn,
      typename Slot,
      typename LastSlotClass,
      typename LastSlot>
  static constexpr void checkLastSlotAgainst(LastSlot LastSlotClass::*) {
    static_assert(
        std::derived_from<LastSlotClass, ContainedIn>,
        "Verify your `static constexpr auto LastSlot = &YourClass::var_;`");
    static_assert( // Slot's position <= LastSlot's position
        std::tuple_size_v<decltype(Slot::prevSlots)> <=
        std::tuple_size_v<decltype(LastSlot::prevSlots)>);
  }

  template <typename ContainedIn, typename Slot>
  static constexpr bool checkClassAndSlot() {
    static_assert(std::derived_from<ContainedIn, AsyncObject>);
    checkLastSlotAgainst<ContainedIn, Slot>(ContainedIn::LastSlot);
    return true;
  }

  // A slot knows its `ContainedIn` class & predecessor `PtrToPrevSlot`.
  // This means that once the class is complete, we can look up this slot's
  // own pointer-to-member by working backwards from `LastSlot`.
  template <
      typename SearchInObj,
      auto Needle, // nullptr (first slot), or PtrToPrevSlot of slot to find
      typename LastSlotT>
  static FOLLY_CONSTEVAL auto findSlotPtrViaPrev(LastSlotT SearchInObj::*) {
    constexpr size_t numPrevs =
        std::tuple_size_v<decltype(LastSlotT::prevSlots)>;
    if constexpr (numPrevs == 0) {
      return SearchInObj::LastSlot;
    } else {
      if constexpr (Needle == nullptr) {
        return std::get<0>(LastSlotT::prevSlots);
      } else {
        constexpr size_t idx = []() {
          size_t idx = 0;
          std::apply(
              [&](auto&... as) { (((as != Needle) && ++idx) && ...); },
              LastSlotT::prevSlots);
          return idx;
        }();
        if constexpr (idx + 1 == numPrevs) {
          return SearchInObj::LastSlot;
        } else {
          return std::get<idx + 1>(LastSlotT::prevSlots);
        }
      }
    }
  }

 protected:
  ~AsyncObject() = default; // Meant to be a base class only

  struct NoPriorSlot {};
  constexpr static NoPriorSlot noPriorSlot{};

  template <typename, typename, auto>
  class Slot;

  template <typename T, typename ContainedIn>
  class Slot<T, ContainedIn, noPriorSlot> : public SlotBase<T> {
   private:
    friend class AsyncObject;
    template <typename, typename, auto>
    friend class Slot;

    static constexpr std::tuple<> prevSlots;

   public:
    explicit Slot(AsyncObjectTag, auto);
    // XXX test if a public [[FOLLY_ATTR_NO_UNIQUE_ADDRESS]] member would
    //     trigger use-after-move lints ... or at least DCHECK
    // XXX how to make this function single-use? what goes wrong if it's not?
    // XXX sigils need to be consumed via move (+ dcheck) to trigger
    // use-after-move linters
    detail::AsyncObjectSlotTag<ContainedIn, 0> nextTag() { return {}; }

    // Get slot data via `ptr->yourSlot_(ptr)->...`.
    //
    // Rationale: As discussed in the file docblock, `AsyncObject` favors
    // `static` APIs taking the pointer as the first argument.  While
    // redundant, this lets us specialize behaviors depending on how the
    // object is owned.  Specifically here, we:
    //  - Read the slot via `ptr` -- only the **type** of `this` is used.
    //  - Let `SlotLimitedObjectPtr` enforce its constraint, which prevents
    //    cleaned-up slots from being accessed by scope-scheduled coros.
    auto operator()(auto& ptr) const { // mark `static` in C++23
      return slotPtr<findSlotPtrViaPrev<ContainedIn, nullptr>(
          ContainedIn::LastSlot)>(ptr);
    }
  };

  template <
      typename T,
      typename ContainedIn,
      typename PrevClass,
      typename PrevSlot,
      PrevSlot PrevClass::*PtrToPrevSlot>
  class Slot<T, ContainedIn, PtrToPrevSlot> : public SlotBase<T> {
   private:
    friend class AsyncObject;
    template <typename, typename, auto>
    friend class Slot;

    static constexpr auto prevSlots =
        std::tuple_cat(PrevSlot::prevSlots, std::tuple{PtrToPrevSlot});

   public:
    template <typename ClassFromTag, size_t IdxFromTag>
    explicit Slot(detail::AsyncObjectSlotTag<ClassFromTag, IdxFromTag>, auto);

    detail::
        AsyncObjectSlotTag<PrevClass, std::tuple_size_v<decltype(prevSlots)>>
        nextTag() {
      return {};
    }

    // Get slot data via `ptr->yourSlot_(ptr)->...`. Details on `noPriorSlot`.
    auto operator()(auto& ptr) const { // mark `static` in C++23
      return slotPtr<findSlotPtrViaPrev<ContainedIn, PtrToPrevSlot>(
          ContainedIn::LastSlot)>(ptr);
    }
  };

 private:
  // Collect references to all the slots into a tuple.
  template <typename Obj>
  static auto asyncObjectSlots(Obj& obj) {
    if constexpr (requires { Obj::LastSlot; }) {
      return std::apply(
          [&](auto... ptrToPrevSlots) {
            return std::tuple_cat(std::forward_as_tuple(
                (obj.*(ptrToPrevSlots)).slot_...,
                (obj.*(Obj::LastSlot)).slot_));
          },
          // NB: This should be resolved statically
          (obj.*Obj::LastSlot).prevSlots);
    } else {
      return std::tuple{};
    }
  }

  // Quack-alike of `capture`, lets me reuse `detail/AsyncClosure.h` helpers
  template <typename T>
  struct FakeCaptures {
    FakeCaptures(T& obj) : obj_(obj) {}
    using capture_type = T;
    auto& get_lref() { return obj_; }
    T& obj_;
  };

 public:
  // DO NOT USE OR OVERRIDE -- these are NOT public.
  //
  // These invoke `co_cleanup` and `setParentCancelToken`, respectively,
  // both on the parent object itself, AND on its slots.
  //
  // Rationale: This is a private API to support including `AsyncObject`s as
  // `async_closure()` arguments.  It solves these requirements:
  //    - `co_cleanup` and `setParentCancelToken` are guaranteed to be
  //      invoked for all `Slots`, regardless of whether (or how) derived
  //      classes implement these protocols.
  //    - Objects made via `asyncObjectPtr` and `asyncObjectAsArg` have
  //      identical `co_cleanup` and `setParentCancelToken` behaviors.
  //
  // The rejected alternative was to have `co_cleanup` and
  // `setParentCancelToken` on the `AsyncObject` base fan out to the slots.
  // Then, classes deriving from `AsyncObject`, and overriding one of the
  // protocols would have to remember to ALSO call the base method, in
  // addition to any custom logic.  "Failed to delegate to base" bugs could
  // be detected by a linter, but handling inheritance goes across
  // compilation units, and is therefore quite hard to pull off in practice.
  static void privateHackSetParentCancelToken(
      auto& me, async_closure_private_t priv, const CancellationToken& ctok) {
    std::apply(
        [&](auto&&... args) {
          (detail::async_closure_set_cancel_token(priv, args, ctok), ...);
        },
        AsyncObject::asyncObjectSlots(me));
    detail::async_closure_set_cancel_token(priv, FakeCaptures{me}, ctok);
  }
  // Ugly name so that grepping for both `privateHack` & `co_cleanup` finds it
  static auto privateHack_co_cleanup(
      auto&& me, async_closure_private_t priv, const exception_wrapper* err) {
    return std::apply(
        [&](auto&... args) {
          return std::tuple_cat(
              detail::async_closure_make_cleanup_tuple(priv, args, err)...,
              detail::async_closure_make_cleanup_tuple(
                  priv, FakeCaptures{me}, err));
        },
        AsyncObject::asyncObjectSlots(me));
  }

  // This part could have been a separate base, `AsyncObjectBaton`
 private:
  template <std::derived_from<AsyncObject>>
  friend class AsyncObjectPtr;
  template <std::derived_from<AsyncObject> T>
  friend AsyncObjectPtr<T> asyncObjectPtr(
      detail::is_SafeAsyncScopeContextProxy auto, auto&&...);

  struct PostCleanupBaton {
    void operator()(AsyncObject* p) { p->startCleanup_.post(); }
  };

  folly::coro::Baton startCleanup_;
};

// This private tag restricts `AsyncObject`s and their `Slot`s to only be
// constructible in contexts that safely manage their lifetime & cleanup.
//
// Copying is forbidden to ensure that only one `Slot<..., noPriorSlot>` can
// be initialized with this tag without triggering use-after-move lints.
//
// Future: Add `DCHECK` enforcing the "one `noPriorSlot` rule".
class AsyncObjectTag : MoveOnly {
 private:
  template <std::derived_from<AsyncObject>>
  friend auto asyncObjectAsArg(auto&&...); // for ctor
  template <std::derived_from<AsyncObject> T>
  friend AsyncObjectPtr<T> asyncObjectPtr( // for ctor
      detail::is_SafeAsyncScopeContextProxy auto,
      auto&&...);
  template <typename, size_t>
  friend class detail::AsyncObjectSlotTag; // for ctor
  template <detail::has_async_closure_co_cleanup>
  friend class AsyncObject::SlotBase; // for `capture_private`

  static constexpr detail::capture_private_t capture_private{};
  static constexpr async_closure_private_t async_closure_private{};

  AsyncObjectTag() = default;
};

namespace detail {

// Move-only for the same reason that `AsyncObjectTag` is.
// Future: Add `DCHECK` enforcing that "slot tags" are single-use.
template <typename PrevClass, size_t Idx>
class AsyncObjectSlotTag final : MoveOnly {
 private:
  template <typename, typename, auto>
  friend class AsyncObject::Slot;

  AsyncObjectSlotTag() = default;

  constexpr AsyncObjectTag asyncObjectTag() && { return {}; }
};

// TODO: Add a version for `restricted_co_cleanup_capture` objects that puts
// a `restricted_co_cleanup_capture<Slot&>` into the `SlotProxy` -- e.g. by
// adding a `constexpr` member.  or returnimg a specially wrapped reference,
// for `slotPtr()` to detect.
template <std::derived_from<AsyncObject> T>
struct AsyncObjectRefForSlot<co_cleanup_capture<T>> {
  template <auto>
  static constexpr decltype(auto) validateAndGetObjectRef(auto& objArg) {
    return objArg.get_lref();
  }
};

template <typename T>
struct AsyncObjectRefForSlot<AsyncObjectPtr<T>> {
  template <auto>
  static constexpr decltype(auto) validateAndGetObjectRef(auto& objPtr) {
    return *objPtr;
  }
};

} // namespace detail

template <detail::has_async_closure_co_cleanup T>
AsyncObject::SlotBase<T>::SlotBase(AsyncObjectTag tag, auto&& t)
    : slot_(
          tag.capture_private,
          std::get<0>(folly::bindings::ensure_binding_tuple(
              std::forward<decltype(t)>(t)))) {}

template <typename T, typename ContainedIn>
AsyncObject::Slot<T, ContainedIn, AsyncObject::noPriorSlot>::Slot(
    AsyncObjectTag tag, auto t)
    : SlotBase<T>(std::move(tag), std::forward<decltype(t)>(t)) {
  static_assert(checkClassAndSlot<ContainedIn, Slot>());
}

template <
    typename T,
    typename ContainedIn,
    typename PrevClass,
    typename PrevSlot,
    PrevSlot PrevClass::*PtrToPrevSlot>
template <typename ClassFromTag, size_t IdxFromTag>
AsyncObject::Slot<T, ContainedIn, PtrToPrevSlot>::Slot(
    detail::AsyncObjectSlotTag<ClassFromTag, IdxFromTag> prevSlotTag, auto t)
    : SlotBase<T>(
          std::move(prevSlotTag).asyncObjectTag(),
          std::forward<decltype(t)>(t)) {
  static_assert(std::is_same_v<PrevClass, ClassFromTag>);
  static_assert(std::tuple_size_v<decltype(prevSlots)> == IdxFromTag + 1);
  static_assert(std::derived_from<ContainedIn, PrevClass>);
  static_assert(checkClassAndSlot<ContainedIn, Slot>());
}

// Provides `unique_ptr`-like, deep-`const` access to the underlying
// `AsyncObject` until the pointer is destroyed, moved out, or reset.
// Thereafter, the parent scope is told to start async cleanup, and
// eventually destroy the object.
//
// Besides RAII, `AsyncObjectPtr` is identical in functionality and safety
// to an `co_cleanup_capture<T&>`.  Keep them remain API-interchangeable,
// except for nullability (`operator bool`, `reset` etc), which results from
// `AsyncObjectPtr` having "owning" semantics, and being movable.
//
// Future 1: By the same token, we will likely want `operator=`.
//
// Future 2: A use-case for these ptrs is to move one into a sub-closure to
// obtain a `SafeTask<shared_cleanup>`, which may be passed into another
// `async_closure`.  The trouble is we can't return them (notes on
// `safe_alias_for` below).  One workaround would be to make a shared
// version.  Another, without atomic overhead, would be to implement borrow
// semantics as follows:
//   auto [lender, loanPtr] = std::move(ptr).borrow();
//   ptr = std::move(lessor).repossess(
//       co_await async_closure(..., std::move(loanPtr)));
// Design notes:
//   - `LoanPtr` owns the ptr, and will destroy it.  Contrast this to the
//     unsafe design where `~LoanPtr()` mutates `ptr`.
//   - `Lender` is non-movable to avoid messy moved-out states.  Making it
//     copy-only may be a tolerable extension, since only one lender could
//     repossess the unique loan.
//   - `Lender` storess an **inaccessible** copy of the raw pointer,
//     to support a runtime check that it's repossessing the right loan.
//     This `DFATAL` / return null check in `repossess()` is the worst
//     part of the design, but the next bullet can help
//   - `borrow()` can auto-derive a type tag from `source_location`, which
//     would give `repossess()` a specific, but fallible compile-time check
//     that it's got the right loan.  If implementing this, be sure to
//     provide a "user-supplied" type tag overload as below, since
//     source-derived tags cannot be manually declared, line #s change.
// constexpr auto tag(Tag t = {std::source_location::current()}) { return t; }
// template <typename T> constexpr auto tag(T t) { return t; }
//
// Future 3: A `toCaptures()` method that makes a binding that moves a ptr
// into a sub-closure to get an `co_cleanup_capture<Obj&>` inside.
// (1) Supports passing refs into sub-closures, which is much easier
//     than moving Ptrs or LoanPtrs.
// (2) Combined with `AsyncObject` support for restricted refs and
//     non-cleanup objects, objects restricted ref support, it would be even
//     better, since sub-closures could avoid the `shared_cleanup` arg
//     safety downgrade.
// There is no direct conversion from `AsyncObjectPtr` to `capture`s,
// since the latter have a guaranteed lifetime of "at least the current
// closure".
template <std::derived_from<AsyncObject> T>
class AsyncObjectPtr {
 public:
  // `true` if operators `*` and `->` can be used.
  explicit operator bool() const noexcept { return p_; }

  // Notify the object's async cleanup to begin. Not thread-safe.
  // Post-condition: operators `*` and `->` can no longer be used.
  void reset() noexcept { p_.reset(); }

  // Unlike `unique_ptr`, this has "deep `const`" semantics.
  [[nodiscard]] constexpr T& operator*() noexcept { return *p_; }
  [[nodiscard]] constexpr T* operator->() noexcept { return p_.get(); }
  [[nodiscard]] constexpr const T& operator*() const noexcept { return *p_; }
  [[nodiscard]] constexpr const T* operator->() const noexcept {
    return p_.get();
  }

 protected:
  template <std::derived_from<AsyncObject> U>
  friend AsyncObjectPtr<U> asyncObjectPtr(
      detail::is_SafeAsyncScopeContextProxy auto, auto&&...);

  explicit AsyncObjectPtr(T* p) : p_(p) {}

 private:
  // Posts the cleanup baton instead of deleting the object.
  std::unique_ptr<T, AsyncObject::PostCleanupBaton> p_;
  static_assert(sizeof(p_) == sizeof(T*));
};

namespace detail {

// Returning `auto` is tatamount to pass-by-value -- however, the user can
// still mess up the signature of their ctor (by using an lvalue ref, e.g).
// Future: Lint that a ctor of a class deriving from `AsyncObject` takes
// `AsyncObjectTag` as the first arg, and only takes args by value.
auto bindAsyncObjectArg(detail::capture_private_t priv, auto&& arg) {
  if constexpr (is_any_capture<std::remove_cvref_t<decltype(arg)>>) {
    // This part here is why we need preprocess `AsyncObject` args.  We have
    // special knowledge that lets us safely create `co_cleanup_safe_ref`
    // `capture` refs.  "Is shared cleanup?" is false, since all args arge
    // guaranteed to have `>= co_cleanup_safe_ref` safety:
    //   - When on the `asyncObjectPtr` code path, because we schedule
    //     cleanup & destruction on a `SafeAsyncScope`.
    //   - When owned by an `async_closure`, because `AsyncObject` typically
    //     has cleanup (either itself, or via slots).  Some closure cleanup
    //     may run before the object's cleanup, so `< co_cleanup_safe_ref` refs
    //     are unsafe -- they could point at already-cleaned-up data. Future: We
    //     could relax this for cleanup-free objects, but it's work.
    return std::forward<decltype(arg)>(arg)
        .template to_capture_ref</*shared cleanup*/ false>(priv);
  } else {
    return std::forward<decltype(arg)>(arg);
  }
}

// To share logic, this object's API quacks like `async_closure_storage`
template <typename T>
struct AsyncObjectStorage {
  AsyncObjectStorage(auto&&... as)
      : inner_err_(async_closure_default_inner_err()),
        obj_(std::forward<decltype(as)>(as)...) {}

  auto storage_tuple_like() { return std::forward_as_tuple(obj_); }
  auto* inner_err_ptr() { return &inner_err_; }

  exception_wrapper inner_err_; // See docs in `async_closure_storage`
  T obj_;
};

} // namespace detail

// Creates an `AsyncObject` as if `as_capture(make_in_place<T>(...))` so
// that it can be stored as an arg of an `async_closure()`, and cleaned up
// when the closure's outer coro awaits the cleanup of its args.
template <std::derived_from<AsyncObject> T>
auto asyncObjectAsArg(auto&&... args) {
  // Do not directly include `AsyncObjectTag` in the binding, so that it
  // cannot be instantiated directly.  This is by-design -- creating
  // unmanaged `AsyncObject`s violates safety guarantees.  The tag is
  // constructed by special logic in `store_capture_binding`.
  return as_capture(folly::bindings::make_in_place<T>(
      [](detail::capture_private_t) { return AsyncObjectTag{}; },
      std::forward<decltype(args)>(args)...));
}

// Creates an RAII unique pointer to an `AsyncObject`, whose dtor triggers
// async destruction on the provided scope (pass `scope->with(executor)`).
//
// Does NOT take `folly::bindings` or `as_capture` modifiers, see docblock.
template <std::derived_from<AsyncObject> T>
AsyncObjectPtr<T> asyncObjectPtr(
    detail::is_SafeAsyncScopeContextProxy auto dtorScopeCtx, auto&&... args) {
  using ObjArgs = tag_t<decltype(detail::bindAsyncObjectArg(
      AsyncObjectTag::capture_private, std::forward<decltype(args)>(args)))...>;
  constexpr auto minArgSafety = safe_alias_of_v<ObjArgs>;
  static_assert( // Redo `SafeAsyncScope`'s check with a nicer error.
      minArgSafety >= safe_alias::co_cleanup_safe_ref ||
          std::is_void_v<ObjArgs>,
      "`AsyncObject` ctor args need >= co_cleanup_safe_ref safety. See `SafeAlias.h`");

  // Future: It might be worth allocating this on the coro frame to save an
  // allocation.  This would require a custom coro class.  This might also
  // improve `bad_alloc` safety by delaying object construction until after
  // the allocations happen.  Details in the file docblock.
  auto storagePtr = std::make_unique<detail::AsyncObjectStorage<T>>(
      AsyncObjectTag{},
      detail::bindAsyncObjectArg(
          AsyncObjectTag::capture_private,
          std::forward<decltype(args)>(args))...);

  T* objPtr = &storagePtr->obj_;
  auto ctok =
      std::move(dtorScopeCtx)
          .scheduleReturningCancelToken(
              AsyncObjectTag::async_closure_private,
              // The `false` here says: do NOT `setParentCancelToken` once
              // awaiting the outer coro -- we are about to do it below.
              async_closure_make_outer_coro<false, void, minArgSafety>(
                  AsyncObjectTag::async_closure_private,
                  // Await a `Baton` as the "inner coro".
                  objPtr->startCleanup_.operator co_await(),
                  std::move(storagePtr)));
  // As discussed in `WithParentCancelToken`, we could avoid an extraneous
  // token copy/destruction (2 atomic ops) with the `Task` API extension in
  // https://fburl.com/ctok_stable_ptr.
  AsyncObject::privateHackSetParentCancelToken(
      *objPtr, AsyncObjectTag::async_closure_private, ctok);

  return AsyncObjectPtr<T>{objPtr};
}

} // namespace folly::coro

namespace folly::detail {
// Used in synchronous code, these are as safe as `unique_ptr`, but in
// `async_closure()` we need some restrictions.  They arise because, as an
// `async_closure` arg, this behaves like an `co_cleanup_capture<T>`.
// (1) Moving a `Ptr` an `async_closure()` lets it run code on the scopes
//     (and other cleanup args) that live outside the closure, the cleanup
//     of an outer async scope, so we need `shared_cleanup`.
// (2) Moving the pointer out of the lexical scope of its "destructor async
//     scope" is guaranteed to cause a deadlock on `join`.  This constraint
//     is weaker than (1), `<= body_only_ref` prevents deadlock:
//       - Cannot return it from a closure.
//       - Cannot pass it to a sub-closure being scheduled on a scope.
// (3) Since we're moving the ptr inside the closure, there's no risk
//     that it will be destroyed in the parent while the closure is running.
template <typename T>
struct safe_alias_for_<::folly::coro::AsyncObjectPtr<T>>
    : safe_alias_constant<safe_alias::shared_cleanup> {};
} // namespace folly::detail
