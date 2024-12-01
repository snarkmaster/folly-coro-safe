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

#include <folly/Traits.h>
#include <folly/Utility.h>
#include <folly/coro/safe/AsyncClosure-fwd.h>
#include <folly/coro/safe/SafeAlias.h>
// `#undef`ed at end-of-file not to leak this macro.
#include <folly/coro/safe/detail/DefineMovableDeepConstLrefCopyable.h>
#include <folly/lang/Assume.h>
#include <folly/lang/Bindings.h>

#ifndef _WIN32 // Explained in SafeTask.h

// XXX distill docblock from mess in CapturesDoc.h
// XXX Touch on copyability / movability of types?
/*
Exists for two reasons:
(1) `capture<T>` uarantees that underlying value is lifetime-managed, and
    will get cleaned up.
(2) Corresponding safe_alias tagging.

What the user should know:
  - Use `by_ref` to pass `capture` to sub-closures or
    `SafeAsyncScopeProxy::add` & friends
  - The `capture` value is meant to stay on the originating closure, since
    all `capture` reference safety is based on this. There are some
    scenarios where it makes sense to move a value, but you must be
    very careful that none of the refs / callbacks with refs are going
    to access the moved-out value.

Don't do this / potential lints:
  - Avoid passing `capture`s (or other refs, for that matter) into
    callables (or class instance functions) that came from outside
    the current closure -- there are no safety checks on these.
  - While moving arg values is discouraged (above), if you do, AVOID
    `std::move(*arg)`.  Prefer `*std::move(arg)` since this supports
    use-after-move linting, and (potentially) future debug runtime checks.
  - Since `capture` relies on `safe_alias`, avoid placing
    `capture` refs into containers that lack `safe_alias_for_`.

Existing compile-time safeguards:
  - Passing `capture<Ref>`s to sub-closures & SafeAsyncScopeProxy::add*
    only works when safe
  - Returning refs should fail
  - The `safe_alias` tags discourage passing `capture<Val>`s.
  - `SafeTask` taking `capture`s are (of course) tained by their safety.

--- XXX ---
`AsyncOuterClosurePtr<T>` stores a `T` in the closure's "outer" storage,
which will outlive the inner coro of an `async_closure()`.  Sample usage:
  co_await async_closure(
    [](auto t) -> Task<int> { return t->computeInt(); },
    async_closureInPlace<T>(args...));

Note the inner task sees a pointer-like `t`.
  - XXX `as_capture()` is a helper to copy or move values into the
    closure's "outer" storage.
  - You can also store a const value via `as_capture(make_in_place<const
T>())`.
  - If you prefer, `const` can be redundantly added to the argument -- but
    this will not compile without the construction-time marking.

It has a few uses:
 - Store data on the outer scope of your `async_closure()`.  This lets it be
   safely (as in `SafeAlias.h`) referenced by child coroutines of your
   `async_closure()`.  In contrast, regular arguments are not guaranteed to
   outlive closure cleanup -- e.g. awaitables on inner `SafeAsyncScope`s.
 - Pass non-movable, non-copyable types "for free" by piggybacking on the
   `async_closure()`'s internal heap allocation.
 - Help implement types that support async cleanup via `async_closure()`.

Rationale: Why are these `Ptr`s?  Morally, these are references, and the
"pointer" type UX reduces ergonomics.
  - Plain references could work, but doing so would leak implementation
    details into user-supplied generic lambdas. They would also make
    it harder to evolve the implementation.  `SaferCoro.md` justifies
    this in "why `auto` is best practice for generic safe coros".
  - Unfortunately, C++ doesn't really have a good story for
    transparent reference-like wrapper types, see e.g.
    https://medium.com/@snowp/transparent-phantom-types-in-c-de6ac5bed1d1
*/

namespace folly {
class CancellationToken;
class exception_wrapper;
} // namespace folly

namespace folly::coro {

class AsyncObjectTag;

template <safe_alias, typename>
class SafeTask;

namespace detail {

template <typename>
struct AsyncObjectRefForSlot;

template <typename T>
concept has_async_closure_co_cleanup_error_oblivious =
    requires(T t, async_closure_private_t p) { std::move(t).co_cleanup(p); };
template <typename T>
concept has_async_closure_co_cleanup_with_error =
    requires(T t, async_closure_private_t p, const exception_wrapper* e) {
      std::move(t).co_cleanup(p, e);
    };
template <typename T> // DO NOT USE: for AsyncObject only
concept has_async_object_private_hack_co_cleanup =
    requires(T t, async_closure_private_t p, const exception_wrapper* e) {
      t.privateHack_co_cleanup(std::move(t), p, e);
    };
template <typename T>
concept has_async_closure_co_cleanup =
    has_async_closure_co_cleanup_error_oblivious<T> ||
    has_async_closure_co_cleanup_with_error<T> ||
    has_async_object_private_hack_co_cleanup<T>;

// Any binding with this key is meant to be owned by the async closure
enum class capture_projection {
  any = 0, // No user-expressed storage preference
  // Syntax sugar: Passing `as_capture_indirect()` with a pointer-like (e.g.
  // `unique_ptr<T>`), this emits a `capture_indirect<>`, giving access to
  // the underlying `T` with just one dereference `*` / `->`, instead of 2.
  indirect,
};

template <typename Base>
struct capture_bind_info : Base {
  capture_projection capture_p;

  using Base::Base;
  constexpr explicit capture_bind_info(Base b, capture_projection ap)
      : Base(std::move(b)), capture_p(ap) {}
};

template <typename B>
concept is_capture_binding = is_instantiation_of_v<
    capture_bind_info,
    std::remove_cvref_t<decltype(B::bind_info)>>;

} // namespace detail

// XXX doc
inline constexpr ::folly::bindings::detail::ensure_binding_tuple_with_<
    [](std::derived_from<folly::bindings::detail::bind_info> auto bi) {
      return detail::capture_bind_info<decltype(bi)>{
          std::move(bi), detail::capture_projection::any};
    }>
    as_capture;
inline constexpr ::folly::bindings::detail::ensure_binding_tuple_with_<
    [](std::derived_from<folly::bindings::detail::bind_info> auto bi) {
      return detail::capture_bind_info<decltype(bi)>{
          std::move(bi), detail::capture_projection::indirect};
    }>
    as_capture_indirect;

template <typename T>
  requires(!detail::has_async_closure_co_cleanup<T>)
class capture;
template <typename T>
  requires(!detail::has_async_closure_co_cleanup<T>)
class after_cleanup_capture;
template <typename T>
class capture_indirect;
template <typename T>
class after_cleanup_capture_indirect;

// Given an cvref-qualified `capture` type, what `capture` reference
// type is it implicitly converible to?  The input value category affects
// the output reference type exactly as you'd expect for types NOT wrapped
// by `capture`. But, additionally, this knows to pick the correct wrapper:
//   - `co_cleanup_capture` inputs become `co_cleanup_capture<SomeRef>`
//   - `after_cleanup_ref_*` inputs become `after_cleanup_capture<SomeRef>`
//   - everything else becomes just `capture<SomeRef>`
template <typename Captures>
using capture_implicit_ref_t =
    std::remove_cvref_t<Captures>::template ref_like_t<Captures>;

namespace detail {

class capture_private_t {
 public:
  capture_private_t(const capture_private_t&) = default;
  capture_private_t& operator=(const capture_private_t&) = default;

 protected:
  friend struct CapturesTest;
  template <typename, template <typename> class, typename>
  friend class capture_crtp_base;
  template <typename, auto>
  friend class capture_binding_helper;
  template <bool>
  friend auto bind_captures_to_closure(auto, auto&&...);
  friend constexpr capture_private_t coro_safe_detail_bindings_test_private();
  friend class ::folly::coro::AsyncObjectTag;
  capture_private_t() = default;
};

struct capture_restricted_tag {};

template <typename Derived, template <typename> class RefArgT, typename T>
class capture_crtp_base {
 private:
  static constexpr bool has_get_lref = requires(Derived& d) { d.get_lref(); };

  static constexpr auto assert_result_is_non_copyable_non_movable(auto&& fn) {
    using U = decltype(fn());
    // Tests `U&` instead of `is_copy_*` to catch non-regular classes that
    // declare a U(U&) ctor.  This implementation is for class types only.
    static_assert(
        !(std::is_constructible_v<U, U&> || std::is_constructible_v<U, U&&> ||
          std::is_assignable_v<U&, U&> || std::is_assignable_v<U&, U&&>),
        "When a class provides custom dereferencing via `capture_*_proxy`, "
        "it must be `NonCopyableNonMovable` to ensure that it can only passed "
        "via `capture<Ref>`, not via your temporary proxy object. The goals "
        "are (1) ensure correct `safe_alias_of_v` markings, (2) keep the "
        "forwarding object as a hidden implementation detail.");
    return fn();
  }

  // Object intended for use with `capture`  (like `SafeAsyncScope`) may
  // provide overloads of the helper functions `capture_lref_proxy` and
  // `capture_ptr_proxy` to provide custom "lvalue reference" / "pointer"
  // types for `capture` operators `*` and `->`.
  //
  // We don't allow rvalue refs to cleanup args, so this has no rref
  // counterpart.
  //
  // The reason for this indirection is as follows:
  //   - "Restricted" references to scopes must enforce stricter
  //     `safe_alias_of_v` constraints on their awaitables.
  //   - A `restricted_co_cleanup_capture<Ref>` may be obtained from an
  //     `co_cleanup_capture<...AsyncScope...>` that was originally NOT
  //     restricted -- so, "restricted" is a property of the reference, not
  //     of the underlying scope object.
  //   - Therefore, the public API of `SafeAsyncScope` must sit in a
  //     "reference" object that knows if it's restricted, not in the storage
  //     object (which does not).
  //   - It would break encapsulation to put `AsyncScope`-specific logic like
  //     `add` / `schedule` / `schedule*Closure` into `Captures.h`.
  //
  // A type will not be accessible via `restricted_co_cleanup_capture` unless
  // it provides overloads for `capture_restricted_lref_proxy` and
  // `capture_restricted_ptr_proxy`.  There's no default behavior for
  // restricted refs, because the underlying class needs to implement strong
  // enough safety constraints that the ref can be `after_cleanup_ref`.
  static constexpr decltype(auto) get_lref_proxy(auto& self) {
    auto& lref = self.get_lref();
    if constexpr (std::is_base_of_v<capture_restricted_tag, Derived>) {
      return assert_result_is_non_copyable_non_movable([&]() {
        return capture_restricted_lref_proxy(capture_private_t{}, lref);
      });
    } else if constexpr ( // Custom dereference
        requires { capture_lref_proxy(capture_private_t{}, lref); }) {
      return assert_result_is_non_copyable_non_movable(
          [&]() { return capture_lref_proxy(capture_private_t{}, lref); });
    } else {
      return lref; // Default
    }
  }
  static constexpr decltype(auto) get_ptr_proxy(auto& self) {
    auto& lref = self.get_lref();
    if constexpr (std::is_base_of_v<capture_restricted_tag, Derived>) {
      return assert_result_is_non_copyable_non_movable([&]() {
        return capture_restricted_ptr_proxy(capture_private_t{}, lref);
      });
    } else if constexpr ( // Custom member access
        requires { capture_ptr_proxy(capture_private_t{}, lref); }) {
      return assert_result_is_non_copyable_non_movable(
          [&]() { return capture_ptr_proxy(capture_private_t{}, lref); });
    } else {
      return &lref; // Default
    }
  }

  // Invokes a callable, ensuring its return value is of type `Expected`,
  // while retaining prvalue semantics.
  template <typename Expected>
  static constexpr auto assert_return_type(auto fn) {
    static_assert(std::is_same_v<decltype(fn()), Expected>);
    return fn();
  }

 public:
  using capture_type = T;

  // Implement operators `*` and `->` for lvalue `capture` types.  These
  // operators have a separate, destructive implementation for rvalue refs,
  // for reasons discussed in `capture_storage<Derived, RefArgT, V&&>`.
  [[nodiscard]] constexpr decltype(auto) operator*() noexcept
    requires has_get_lref
  {
    return get_lref_proxy(*static_cast<Derived*>(this));
  }
  [[nodiscard]] constexpr decltype(auto) operator->() noexcept
    requires has_get_lref
  {
    return get_ptr_proxy(*static_cast<Derived*>(this));
  }
  [[nodiscard]] constexpr decltype(auto) operator*() const noexcept
    requires has_get_lref
  {
    return get_lref_proxy(*static_cast<const Derived*>(this));
  }
  [[nodiscard]] constexpr decltype(auto) operator->() const noexcept
    requires has_get_lref
  {
    return get_ptr_proxy(*static_cast<const Derived*>(this));
  }

  // Private implementation detail -- public users should instead use the
  // below implicit conversions.  This is how `async_closure()` (and
  // similar) create a matching `capture<Ref>` from a `Derived` instance.
  // The resulting type is `RefArgT`, except for the narrow case when a
  // non-`shared_cleanup` closure is converting a `after_cleanup_ref_`
  // input.
  //   - `Derived::capture_type` may be a value or a reference
  //   - `T` may be a value or reference
  // The main reason `to_capture_ref` is locked down is that when
  // `SharedCleanupClosure == false`, we upgrade `after_cleanup_ref_` refs.
  // This is unsafe to do unless we know that the ref is going into
  // an independent, nested async scope.
  template <bool SharedCleanupClosure>
  auto to_capture_ref(capture_private_t) & {
    return to_capture_ref_impl<SharedCleanupClosure>(
        static_cast<Derived&>(*this).get_lref());
  }
  template <bool SharedCleanupClosure>
  auto to_capture_ref(capture_private_t) const& {
    return to_capture_ref_impl<SharedCleanupClosure>(
        static_cast<const Derived&>(*this).get_lref());
  }
  template <bool SharedCleanupClosure>
  auto to_capture_ref(capture_private_t) && {
    if constexpr (has_get_lref) {
      return to_capture_ref_impl<SharedCleanupClosure>(
          std::move(static_cast<Derived&>(*this).get_lref()));
    } else {
      return to_capture_ref_impl<SharedCleanupClosure>(
          static_cast<Derived&&>(*this).get_rref());
    }
  }

  // Prefer `capture_implicit_ref_t`, which is easier to use.  Given an
  // instance of this `capture` of cvref category `LikeMe`, which
  // `capture<Ref>` can it be implicitly converted to?
  template <typename LikeMe>
  using ref_like_t = RefArgT<like_t<LikeMe&&, T>>;

  // Implictly convert an capture instance to an capture reference of a
  // matching cvref category.
  //
  // These conversions are provided because we want capture- wrapped types
  // to act much like the underlying unwrapped types.  You can think of this
  // conversion as allowing cvref qualifiers on the wrapper to be moved
  // **inside** the wrapper.  The test shows full coverage, but in essence,
  // the outer reference category replaces the outer one, any `const` moves
  // inside the wrapper, and we never remove a `const` qualifier already
  // present in the wrapper.  Examples:
  //   capture<int>& -> capture<int&>
  //   const capture<int>& -> capture<const int&>
  //   capture<const int&>&& -> capture<const int&&>
  // Use the type function `capture_implicit_ref_t` to find these implicit
  // cast destination types.
  //
  // We also support an explicit rref to lref conversion:
  //   capture<int&&>&& -> capture<int&>
  // The idea here is that you're passing `capture<V&&>` down into a child
  // of your closure.  That deliberately has stricter single-use semantics
  // than `V&&` in vanilla C++ -- for example, without single-use, an rref
  // could be used to move out a value that is still referenced in
  // SafeAsyncScope task.  Having the explicit && -> & conversion permits
  // the child change its mind about moving out the value.
  //
  // Future ideas & implementation notes:
  //   - We may want to support implicitly adding `const`. Today's solution
  //     is to take `const capture`, which should be fine for most usage?
  //   - This (and `to_capture_ref` should technically have a `const&&`
  //     overload, but that's "impact for another day", whenever someone
  //     actually needs it.
  //   - All 3 implicit conversions can  be `operator auto`, but I suspect
  //     this would hurt compile-time.  Benchmark before changing.
  /*implicit*/ operator ref_like_t<int&>() & {
    return assert_return_type<ref_like_t<int&>>([&] {
      return static_cast<Derived&>(*this)
          .template to_capture_ref</*SharedCleanup*/ true>(capture_private_t{});
    });
  }
  /*implicit*/ operator ref_like_t<const int&>() const& {
    return assert_return_type<ref_like_t<const int&>>([&] {
      return static_cast<const Derived&>(*this)
          .template to_capture_ref</*SharedCleanup*/ true>(capture_private_t{});
    });
  }
  /*implicit*/ operator auto() && { // Actually, `operator ref_like_t<int&&>`
    // This has to be `operator auto`, with a "stub" branch for cleanup
    // args, because an `co_cleanup_capture` constraint bans r-value
    // references, preventing us from unconditionally instantiating
    // `ref_like_t<int&&>` for all `capture` types.  An alternate
    // implementation might be for `co_cleanup_capture` to use a
    // `static_assert` instead, but I didn't explore that.
    if constexpr (has_async_closure_co_cleanup<std::remove_cvref_t<T>>) {
      return;
    } else {
      return assert_return_type<ref_like_t<int&&>>([&] {
        return static_cast<Derived&&>(*this)
            .template to_capture_ref</*SharedCleanup*/ true>(
                capture_private_t{});
      });
    }
  }
  // Allow expliictly moving `capture<V&&>` into `capture<V&>`.
  explicit operator ref_like_t<int&>() &&
    requires(std::is_rvalue_reference_v<T>)
  {
    static_assert(!has_get_lref);
    return assert_return_type<ref_like_t<int&>>([&] {
      auto&& rref = static_cast<Derived&&>(*this).get_rref();
      auto& lref = static_cast<std::remove_reference_t<decltype(rref)>&>(rref);
      return to_capture_ref_impl</*SharedCleanup*/ true>(lref);
    });
  }

 private:
  template <bool SharedCleanupClosure, typename V>
  static auto to_capture_ref_impl(V&& v) {
    // If the receiving closure takes no `shared_cleanup` args, then it
    // cannot* pass any of its `capture` refs to an external, longer-lived
    // cleanup callback.  That implies we can safely upgrade any incoming
    // `after_cleanup_ref_` refs to regular post-cleanup `capture` refs --
    // anything received from the parent is `co_cleanup_safe_ref` from the point
    // of view of **this** closure's cleanup args, and it cannot access others.
    //
    // * As always, subject to the `SafeAlias.h` caveats.
    auto b = folly::bindings::detail::ensure_binding(std::forward<V>(v));
    FOLLY_PUSH_WARNING
    // `folly::bindings::detail::ref_binding` uses conservative
    // `lifetimebound` annotations.  In this case the compiler cannot tell
    // that the ref we're actually `return`ing below is just `v`, which is
    // stored on `this` -- thus, safe.
    FOLLY_GNU_DISABLE_WARNING("-Wreturn-stack-address")
    if constexpr (has_async_closure_co_cleanup<V>) {
      return RefArgT<V&&>{capture_private_t{}, std::move(b)};
    } else if constexpr (
        !SharedCleanupClosure &&
        std::is_same_v<RefArgT<V>, after_cleanup_capture<V>>) {
      return capture<V&&>{capture_private_t{}, std::move(b)};
    } else if constexpr (
        !SharedCleanupClosure &&
        std::is_same_v<RefArgT<V>, after_cleanup_capture_indirect<V>>) {
      return capture_indirect<V&&>{capture_private_t{}, std::move(b)};
    } else {
      return RefArgT<V&&>{capture_private_t{}, std::move(b)};
    }
    FOLLY_POP_WARNING
  }
};

// The primary template is for values, with specializations for references.
// Value and lval refs should quack the same, exposing a pointer-like API,
// which (unlike regular pointers or ref wrappers) is deep-const.
//
// The rvalue reference specialization has a nonstandard semantics.  For
// `capture`s, rvalue refs are **single-use**.  Users should only create
// `capture<V&&>` if they intend to move the value, or perform another
// destructive operation.
//
// Why specialize for references instead of storing `T t_;` in a single
// class, and dispatch via SFINAE?  The main reason is that `T t_` wouldn't
// support assignment, since `T = V&` or `T = V&&` could not be rebound.
template <
    typename Derived,
    template <typename>
    class RefArgT,
    typename V> // has reference specializations below
class capture_storage : public capture_crtp_base<Derived, RefArgT, V> {
  static_assert(!std::is_reference_v<V>);

 public:
  constexpr capture_storage(
      capture_private_t,
      auto binding) // Stores a value type
      : v_(std::move(binding).what_to_bind()) {}

 protected:
  template <typename, template <typename> class, typename>
  friend class capture_crtp_base;
  friend void async_closure_set_cancel_token(
      async_closure_private_t, auto&&, const CancellationToken&);
  friend auto async_closure_make_cleanup_tuple(
      async_closure_private_t, auto&&, const exception_wrapper*);
  template <typename> // For the `capture` specializations only!
  friend struct AsyncObjectRefForSlot;

  constexpr auto& get_lref() noexcept { return v_; }
  constexpr const auto& get_lref() const noexcept { return v_; }

  V v_;
};
template <typename Derived, template <typename> class RefArgT, typename V>
class capture_storage<Derived, RefArgT, V&>
    : public capture_crtp_base<Derived, RefArgT, V&> {
 public:
  constexpr capture_storage(
      capture_private_t,
      // XXX IIUC this `lifetimebound` doesn't make much sense when
      // `binding` is passed in by-value? or ever?
      auto&& binding [[clang::lifetimebound]]) // Wraps a ref
      : p_(&std::forward<decltype(binding)>(binding).what_to_bind()) {}

 protected:
  template <typename, template <typename> class, typename>
  friend class capture_crtp_base;
  constexpr auto& get_lref() noexcept { return *p_; }
  constexpr const auto& get_lref() const noexcept { return *p_; }

  V* p_;
};
// This rvalue specialization has an intentional & important deviation in
// semantics:
//   - All the getters require a `&&`-qualified object, i.e.  their intended
//     use is destructive -- you can `*std::move(arg_ref)` once.  Thereafter,
//     use-after-move linters will complain if you reuse the `capture<V&&>`.
//   - Correspondingly, `operator*` returns `V&&` instead of `V&`.
//
// FIXME: The rval specialization currently has a couple of known issues:
//   - It would be more usable (and consistent with `V&`) if this supported
//     move-assignment.  However, there are some tricky issues I haven't had
//     time to sort out with binding temporaries -- i.e.  naively storing
//     `V*` causes the test not to compile.
//   - It might be good to support a runtime check against reuse, in the
//     style of `RValueReferenceWrapper`.  However, in contrast to that
//     class, I might make it DFATAL here.
template <typename Derived, template <typename> class RefArgT, typename V>
class capture_storage<Derived, RefArgT, V&&>
    : public capture_crtp_base<Derived, RefArgT, V&&> {
 public:
  [[nodiscard]] constexpr V&& operator*() && noexcept { return std::move(r_); }
  [[nodiscard]] constexpr V* operator->() && noexcept { return &r_; }
  [[nodiscard]] constexpr const V&& operator*() const&& noexcept {
    return std::move(r_);
  }
  [[nodiscard]] constexpr const V* operator->() const&& noexcept { return &r_; }

  constexpr V&& operator*() const& {
    static_assert(false, "With `capture<T&&> a`, use `*std::move(a)`");
    assume_unreachable();
  }
  constexpr V* operator->() const& {
    static_assert(false, "With `capture<T&&> a`, use `std::move(a)->`");
    assume_unreachable();
  }

  constexpr capture_storage(
      capture_private_t,
      auto&& binding [[clang::lifetimebound]]) // Wraps a ref
      : r_(std::forward<decltype(binding)>(binding).what_to_bind()) {}

 protected:
  template <typename, template <typename> class, typename>
  friend class capture_crtp_base;
  constexpr V&& get_rref() && noexcept { return std::move(r_); }

  V&& r_;
};

template <typename Derived, template <typename> class RefArgT, typename T>
  requires(!std::is_reference_v<T> && !has_async_closure_co_cleanup<T>)
// Since `capture_heap` is owned directly by the inner task, it has to be
// movable to be passed to the coroutine.  But, to stay API-compatible per
// above, it'd be preferable if users did NOT move it.  To help prevent such
// moves, `capture_safety` marks all `capture<Val>`s as `unsafe`.
//
// We deliberately do NOT support moving out the underlying `unique_ptr`
// because heap storage is meant to be an implementation detail, and is not
// intended to be nullable.  A user needing nullability should pass a
// `unique_ptr` either as `capture_indirect` (1 dereference) or `capture` (2).
class capture_heap_storage : public capture_crtp_base<Derived, RefArgT, T> {
 public:
  capture_heap_storage(capture_private_t, auto binding)
      : p_(std::make_unique<T>(std::move(binding).what_to_bind())) {}

 protected:
  template <typename, template <typename> class, typename>
  friend class capture_crtp_base;
  constexpr auto& get_lref() noexcept { return *p_; }
  constexpr const auto& get_lref() const noexcept { return *p_; }

  std::unique_ptr<T> p_;
};

// This is a direct counterpart to `capture_storage` that collapses two
// dereference operations into one for better UX.  There is no need for a
// `capture_heap_indirect_storage`, since this "indirect" syntax sugar only
// applies to pointer types, which are always cheaply movable, and thus
// don't benefit from `make_in_place`.
//
// Similarly, no support for `co_cleanup()` captures since those generally
// aren't pointer-like, and won't suffer from double-dereferences.
template <typename Derived, template <typename> class RefArgT, typename T>
  requires(!has_async_closure_co_cleanup<T>)
class capture_indirect_storage : public capture_storage<Derived, RefArgT, T> {
 public:
  using capture_storage<Derived, RefArgT, T>::capture_storage;

  // These are all intended to be equivalent to dereferencing the
  // corresponding `capture<T>` twice.
  [[nodiscard]] constexpr decltype(auto) operator*() & noexcept {
    return *(capture_storage<Derived, RefArgT, T>::operator*());
  }
  [[nodiscard]] constexpr decltype(auto) operator*() const& noexcept {
    return *(capture_storage<Derived, RefArgT, T>::operator*());
  }
  [[nodiscard]] constexpr decltype(auto) operator*() && noexcept {
    return *(
        std::move(*this).capture_storage<Derived, RefArgT, T>::operator*());
  }
  [[nodiscard]] constexpr decltype(auto) operator*() const&& noexcept {
    return *(
        std::move(*this).capture_storage<Derived, RefArgT, T>::operator*());
  }
  [[nodiscard]] constexpr decltype(auto) operator->() & noexcept {
    return (capture_storage<Derived, RefArgT, T>::operator->())->operator->();
  }
  [[nodiscard]] constexpr decltype(auto) operator->() const& noexcept {
    return (capture_storage<Derived, RefArgT, T>::operator->())->operator->();
  }
  [[nodiscard]] constexpr decltype(auto) operator->() && noexcept {
    return (std::move(*this).capture_storage<Derived, RefArgT, T>::operator->())
        ->operator->();
  }
  [[nodiscard]] constexpr decltype(auto) operator->() const&& noexcept {
    return (std::move(*this).capture_storage<Derived, RefArgT, T>::operator->())
        ->operator->();
  }

  // Unlike other captures, `capture_indirect` is nullable since the
  // underlying pointer type is, too.
  explicit constexpr operator bool() const
      noexcept(noexcept(this->get_lref().operator bool())) {
    return this->get_lref().operator bool();
  }

  // Use these to access the underlying `T`, instead of dereferencing twice.
  decltype(auto) get_underlying() & {
    return capture_storage<Derived, RefArgT, T>::operator*();
  }
  decltype(auto) get_underlying() const& {
    return capture_storage<Derived, RefArgT, T>::operator*();
  }
  decltype(auto) get_underlying() && {
    return std::move(capture_storage<Derived, RefArgT, T>::operator*());
  }
  decltype(auto) get_underlying() const&& {
    return std::move(capture_storage<Derived, RefArgT, T>::operator*());
  }
};

// `capture` refs are only valid as long as their on-closure storage.
// They can be copied/moved, so their `safe_alias` marking is the only thing
// preventing the use of invalid references.  The docs in `enum class
// safe_alias` discuss how safety levels are assigned for closure
// `capture`s.  `async_closure()` invokes `to_capture_ref()` to emit refs
// with the appropriate safety.
//
// Value-type `capture`s SHOULD not be moved outside of the closure that
// owns them.  In fact, they only have to be exposed to user code when the
// closure can be optimized not to have the outer/inner task split.
// Otherwise, their storage is coupled to the outer task's state, and the
// user sees only refs.  To discourage users of "optimized" closures from
// moving args around, and breaking future improvements to closure
// implementations, we mark value types `unsafe_closure_internal` -- a level
// that `async_closure` does NOT consider to be safe.
template <typename T, safe_alias RefSafety>
struct capture_safety
    : safe_alias_constant<
          std::is_reference_v<T> ? RefSafety
                                 : safe_alias::unsafe_closure_internal> {
  using V = std::remove_reference_t<T>;

  // The assertion here expresses two related opinions.
  //  (1) Unsafe types shouldn't be wrapped in `capture` since they're
  //      only intended for `async_closure*()`, which requires safe types.
  //  (2) Types with `co_cleanup(async_closure_private_t)` should, of
  //      course, still be memory-safe.  However, they are implemented
  //      rarely, and should probably not occur at all outside of
  //      `capture`s.  So, they're marked `unsafe` by a generic rule later
  //      in this file to discourage passing them around.
  //
  // Opinion (2) is debatable, and could easily be relaxed -- for example
  // `transform_binding` already bans "regular" non-`capture`s from being
  // "cleanup" args.  If you relax this, check, please also remove the
  // relevant `safe_alias_for_` specialization below.
  static_assert(
      (!has_async_closure_co_cleanup<V> &&
       safe_alias_of_v<V> >= safe_alias::maybe_value) ||
      (has_async_closure_co_cleanup<V> &&
       safe_alias_of_v<V> == safe_alias::unsafe));
};

} // namespace detail

// Please read the file docblock.
//
// Rationale for the move/copy policy of `A = capture<T>`:
//   - When `T` is a ref, `A` must be passed-by-value into coroutines, and
//     so must be at least movable.
//   - Ideally, for value `T`, the args would be permanently attached to the
//     originating closure, but we have to let them be movable so that
//     `async_closure`s without the outer task can own them.  To help prevent
//     this, `capture_safety` marks all `capture<Val>`s as `unsafe`.
//   - Forbid copying for rvalue ref `T` to make use-after-move linters useful.
//     We don't follow `folly::rvalue_reference_wrapper` in adding a runtime
//     `nullptr` check for moved-out refs, but this could be done later.
//   - Allowing copyies of lvalue refs is optional, but helpful.  For
//     example, it lets users naturally pass arg refs into bare sub-tasks.
//     This seems like a reasonable & low-risk thing to do -- our operators
//     already expose refs to the underlying data, so we can't prevent the
//     user from passing `T&` to non-`safe_alias` callables, anyhow.
template <typename T> // may be a value or reference
  requires(!detail::has_async_closure_co_cleanup<T>)
class capture : public detail::capture_storage<capture<T>, capture, T> {
 public:
  FOLLY_MOVABLE_AND_DEEP_CONST_LREF_COPYABLE(capture, T);
  using detail::capture_storage<capture<T>, capture, T>::capture_storage;
};
template <typename T> // may be a value or reference
  requires(!detail::has_async_closure_co_cleanup<T>)
class after_cleanup_capture
    : public detail::
          capture_storage<after_cleanup_capture<T>, after_cleanup_capture, T> {
 public:
  FOLLY_MOVABLE_AND_DEEP_CONST_LREF_COPYABLE(after_cleanup_capture, T);
  using detail::capture_storage<
      after_cleanup_capture<T>,
      after_cleanup_capture,
      T>::capture_storage;
};

// The use-case for `capture_heap` is to allow a closure without cleanup
// args to avoid an inner/outer task split, while still taking
// `make_in_place` arguments.  This is meant to be an implementation detail
// that's almost fully API-compatible with `capture`.  At a future
// point we *could* remove this:
//  - Then, any use of `make_in_place` would auto-create an outer task.
//  - Any user code that explicitly specifies `capture_heap` in signatures
//    would need to be updated to `capture`.
//  - Any places that rely on moving `capture_heap<V>` would need to migrate
//    to `capture_indirect<std::unique_ptr<V>>{}` (which, in contrast, is
//    nullable).  This should be rare, since we mark all value `capture`s as
//    `unsafe` to encourage leaving the value `capture` wrappers in-closure.
template <typename T>
class capture_heap
    : public detail::capture_heap_storage<capture_heap<T>, capture, T> {
 public:
  using detail::capture_heap_storage<capture_heap<T>, capture, T>::
      capture_heap_storage;
};
template <typename T>
class after_cleanup_capture_heap : public detail::capture_heap_storage<
                                       after_cleanup_capture_heap<T>,
                                       after_cleanup_capture,
                                       T> {
 public:
  using detail::capture_heap_storage<
      after_cleanup_capture_heap<T>,
      after_cleanup_capture,
      T>::capture_heap_storage;
};

// `capture_indirect<SomePtr<T>>` is like `capture<SomePtr<T>>` with syntax
// sugar to avoid dereferencing twice.  Use `get_underlying()` instead of
// `*` / `->` to access the pointer object itself.
template <typename T>
class capture_indirect
    : public detail::
          capture_indirect_storage<capture_indirect<T>, capture_indirect, T> {
 public:
  using detail::capture_indirect_storage<
      capture_indirect<T>,
      capture_indirect,
      T>::capture_indirect_storage;
};
template <typename T>
class after_cleanup_capture_indirect : public detail::capture_indirect_storage<
                                           after_cleanup_capture_indirect<T>,
                                           after_cleanup_capture_indirect,
                                           T> {
 public:
  using detail::capture_indirect_storage<
      after_cleanup_capture_indirect<T>,
      after_cleanup_capture_indirect,
      T>::capture_indirect_storage;
};

// A closure that takes a cleanup arg is required to mark its directly-owned
// `capture`s with the `after_cleanup_ref_` prefix, to prevent refs to these
// short-lived args from being passed into longer-lived callbacks.
//
// Don't allow r-value refs to cleanup args, since moving those out of the
// owning closure is unexpected, and probably wrong.
template <typename T> // may be a value or lvalue reference
  requires(!std::is_rvalue_reference_v<T> &&
           detail::has_async_closure_co_cleanup<std::remove_cvref_t<T>>)
class co_cleanup_capture
    : public detail::
          capture_storage<co_cleanup_capture<T>, co_cleanup_capture, T>,
      std::conditional_t<
          !std::is_reference_v<T>,
          folly::NonCopyableNonMovable,
          tag_t<>> {
 public:
  FOLLY_MOVABLE_AND_DEEP_CONST_LREF_COPYABLE(co_cleanup_capture, T);
  using detail::capture_storage<co_cleanup_capture<T>, co_cleanup_capture, T>::
      capture_storage;
};

// Can only schedule `maybe_value` closures, but in exchange does NOT cause
// closures that accept such a ref to mark its own args `after_cleanup_ref` --
// unlike co_cleanup_capture<V&>.
//
// TODO: Note that (IIUC, double-check this & implement) a closure taking a
// restricted ref can choose accept it as a non-restricted ref, since the
// restriction only acts on the sub-closure itself.
template <typename T>
  requires(std::is_lvalue_reference_v<T> &&
           detail::has_async_closure_co_cleanup<std::remove_cvref_t<T>>)
class restricted_co_cleanup_capture : public detail::capture_storage<
                                          restricted_co_cleanup_capture<T>,
                                          restricted_co_cleanup_capture,
                                          T>,
                                      private detail::capture_restricted_tag {
 public:
  FOLLY_MOVABLE_AND_DEEP_CONST_LREF_COPYABLE(restricted_co_cleanup_capture, T);
  using detail::capture_storage<
      restricted_co_cleanup_capture<T>,
      restricted_co_cleanup_capture,
      T>::capture_storage;
};

namespace detail {
template <typename T>
concept is_any_co_cleanup_capture =
    (is_instantiation_of_v<co_cleanup_capture, T> ||
     is_instantiation_of_v<restricted_co_cleanup_capture, T>);
template <typename T>
concept is_any_capture =
    (is_instantiation_of_v<capture, T> ||
     is_instantiation_of_v<capture_heap, T> ||
     is_instantiation_of_v<capture_indirect, T> ||
     is_instantiation_of_v<after_cleanup_capture, T> ||
     is_instantiation_of_v<after_cleanup_capture_heap, T> ||
     is_instantiation_of_v<after_cleanup_capture_indirect, T> ||
     is_instantiation_of_v<co_cleanup_capture, T> ||
     is_instantiation_of_v<restricted_co_cleanup_capture, T>);
template <typename T>
concept is_any_capture_ref =
    is_any_capture<T> && std::is_reference_v<typename T::capture_type>;
template <typename T>
concept is_any_capture_val =
    is_any_capture<T> && !std::is_reference_v<typename T::capture_type>;
} // namespace detail

} // namespace folly::coro

namespace folly::detail {

// Per the note in `capture_safety` above, we want to discourage the
// passing of raw `co_cleanup` types -- they should generally be managed by
// `capture` or similar.  Details in that comment.
template <typename T>
  requires ::folly::coro::detail::has_async_closure_co_cleanup<T>
struct safe_alias_for_<T> : safe_alias_constant<safe_alias::unsafe> {};

// Set `safe_alias` values for all the `capture` types.

template <typename T>
struct safe_alias_for_<::folly::coro::capture<T>>
    : ::folly::coro::detail::
          capture_safety<T, safe_alias::co_cleanup_safe_ref> {};
template <typename T>
struct safe_alias_for_<::folly::coro::capture_heap<T>>
    : ::folly::coro::detail::
          capture_safety<T, safe_alias::co_cleanup_safe_ref> {};
template <typename T>
struct safe_alias_for_<::folly::coro::capture_indirect<T>>
    : ::folly::coro::detail::
          capture_safety<T, safe_alias::co_cleanup_safe_ref> {};

template <typename T>
struct safe_alias_for_<::folly::coro::after_cleanup_capture<T>>
    : ::folly::coro::detail::capture_safety<T, safe_alias::after_cleanup_ref> {
};
template <typename T>
struct safe_alias_for_<::folly::coro::after_cleanup_capture_heap<T>>
    : ::folly::coro::detail::capture_safety<T, safe_alias::after_cleanup_ref> {
};
template <typename T>
struct safe_alias_for_<::folly::coro::after_cleanup_capture_indirect<T>>
    : ::folly::coro::detail::capture_safety<T, safe_alias::after_cleanup_ref> {
};

template <typename T>
struct safe_alias_for_<::folly::coro::co_cleanup_capture<T>>
    : ::folly::coro::detail::capture_safety<T, safe_alias::shared_cleanup> {};
template <typename T>
struct safe_alias_for_<::folly::coro::restricted_co_cleanup_capture<T>>
    : ::folly::coro::detail::capture_safety<T, safe_alias::after_cleanup_ref> {
};

} // namespace folly::detail

// We extended `folly::bindings` with `capture_projection`, so we must
// explicitly specialize `binding_policy`.  We reuse the standard rules.
// Custom `capture` binding logic is in `transform_capture_bindings()`.
namespace folly::bindings::detail {
template <
    folly::coro::detail::capture_bind_info BI,
    can_update_bind_info Binding>
class binding_policy_impl<BI, Binding> {
 private:
  using standard = binding_policy_impl<bind_info{BI}, Binding>;

 public:
  using storage_type = typename standard::storage_type;
  using signature_type = typename standard::signature_type;
};
} // namespace folly::bindings::detail

#endif

#undef FOLLY_MOVABLE_AND_DEEP_CONST_LREF_COPYABLE
