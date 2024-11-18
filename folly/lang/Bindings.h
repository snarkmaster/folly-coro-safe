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

#include <folly/Utility.h>

/// ## Purpose of this utility
///
/// Enable functions to customize how they bind arguments.  Key features:
///   - Coherent with regular C++ argument-binding -- low reader surprisal.
///   - Safer: Enforces two safety-oriended deviations from normal C++:
///       * Callers must wrap `const`-reference args in `by_ref()`.
///         Reduces risk of dangling refs.
///       * Output reference args require `by_non_const_ref()`.
///         Reduces aliasing bugs.
///   - Small & obvious user vocabulary: for the vast majority of usage,
///     `by_ref()` & `by_non_const_ref()` are all you need.
///   - Can bind non-movable, non-copyable args via `make_in_place<T>()`.
///   - Extensible: see e.g. `folly/lang/named` kwargs, `folly/coro/safe`
///     async closure args.
///
/// ## User docs
///
/// You want to call a library that uses `folly::binding`?  No problem,
/// typical usage should Just Work. Nonetheless, please read this:
///  - Binding semantics to know:
///     * Without modifiers: by-value, non-`const`
///     * `by_ref()`: by-`const`-reference, follows C++ ref-collapsing rules.
///     * `by_non_const_ref()`: for output parameters.
///     * More modifiers below.
///  - Do NOT store objects returned by "Binding.h" functions.  They contain
///    refs and must NOT outlive the statement that created them.
///  - Check your library's docs for custom modifiers (e.g. named-arg support).
///
/// ## Library author docs
///
/*
XXX port docs from BindingsRework.h

commutativity does constrain this, this only pertains to const x ref though
incompatibility enforcement does NOT this
 inPlace mutex asRef (standard policy can enforce)
 asyncArg mutex asRef (asyncclosure policy can enforce)
 nullable requires asyncArg (easy, fn can just fail to apply)
  => implies nullable mutex asRef (asyncclosure policy could also enforce
explicitly)

Overall data flow:
  Input (refs -> Type): default / `inPlace()` -- incompatible with `asRef`
    + Projection: `asRef()`, `asNonConst(asRef())` aka `asNonConstRef()`, etc
   -> Policy (late-bound)
      |-> StorageType (what is constructed)
      |-> SignatureType (what the callee sees)

Inputs:
  - default (forwarding reference)
  - `inPlace()`: incompatible with `asRef()`

Projection modifiers: These are functions that take an argument pack, and
return a `BindingList` of new or modified bindings.  Some modifiers nest
with others, e.g. `asNonConstRef()` is short for `asNonConst(asRef())`.
  - Typical usage with the standard policy (below) just needs `asRef()` /
    `asNonConstRef`.  The big win over standard C++, where by-reference args
    are not visible at the callsite, is that dangling ref bugs are easier to
    spot.
  - `NamedBindings.h` add the `"varName"_id = ...` modifier syntax for
    adding tag types to bindings.
  - `AsyncClosure` adds `nullable()`, which is incompatible with `asRef()`,
    and nests with `nullable()`.
  - Explicit `bind::copy()` & `move()` modifiers are naturally supported by
    the standard policy.  They'd need to nest with `nullable` and reject
    `inPlace()`.  They would e.g. enable functions that ban implicit
    copies, or default to by-reference arguments. TAKE CARE: If you implement
    such functions, their name should make the nonstandard semantics clear.
  - For named scopes, we may want to store `const T`, which requires a
    nestable `asConst` modifier, but it's not a very useful feature since
    this renders the scopes non-movable.

Projection states:
  - Category: ref / value / copy / move / default
  - Constness: const / non-const / default -- never removes `const` from input
Having a separate "default" state here allows policies to customize the
behavior while keeping the modifier implementations maximally simple.  On
the other hand, to keep most binding machinery generic, "custom" binding
variations should always default to "off" -- otherwise `ensureAnyBinding`
wouldn't know what to do.  Example variations:
  - (Named) IdTag: adds a tag type for use with named scopes.
  - (AsyncClosure) AsyncArg: yes / default-no
  - (AsyncClosure) Nullability: yes / default-no

Standard policy for composing `StorageType` from `Input::Type`:
  * Category X Constness:
    - The default category is "value".
    - If value/copy/move:
      * Default to storing without `const` (we own it), but (future?)
        `asConst` can add `const`.
      * `value` moves from rvalue ref, copies from lvalue ref
      * `copy` rejects rvalue ref (and the modifier casts it to lvalue)
      * `move` rejects lvalue ref (and the modifier applies `std::move`)
    - If ref:
      * Assert the input is not `InPlace`.
      * Add `const` to the base type, unless `non-const` is explicitly set.
        `non-const` will fail to compile if the base type is `const`.
      * Compose `&&` with the base type (& -> &, value or && -> &&).
In the standard policy, `SignatureType = StorageType`.

`NamedBindings.h` simply extends the standard policy:
  * Adds IdTag to the projection. Then,
    `SignatureType = IdType<"varName", StorageType>`.

Refer to XXX for how `AsyncClosure` incorporates the additional projection
states mentioned above.





inline auto asyncClosureInPlace(auto&&... args [[clang::lifetimebound]]) {
  return detail::AsyncClosureArgMaker<

XXX somewhat dated, revise Principles & conventions:
(1) The callsite determines the binding style, the callee can either be
    flexible by accepting `auto`, or can constrain the binding by
    specifying a type signature.
(2) Bindings are a universal way for users to state intent, but the storage
    / argument-passing implementation is up to the function being called,
    which can vary so long as user intent is honored.  For example, the user
    might pass `asNonConstRef(intVal)` but the implementation may internally
    translate that to an `ArgRef<int>` wrapper class.  If the user spec
    implies `const`, the implementation must store `ArgRef<const int>`.
(3) Functions using `Bindings.h` should be default-const unless the name of
    the function has `NonConst`.  I.e. `foo()` is default-const, unlike
    `fooNonConst()`.  Rationale for exposing this at the callsite:
      - For reference args, it's important that the allsite make it clear
        whether it's an input or an output.
XXX can resolve this thus:
 - default-const for ref, mutable for val (unless `const auto`)
 - REWRITE THIS XXX:
      - For value args, one might prefer to only express `const` in the
        callee.  The primary reason we can't is that C++ doesn't play well
        with `const auto` args -- if such an arg is specialized to a
        reference type, the `const` is lost.
      - Programming best practice is to minimize "implicit" mutation.  When
        writing modern C++, two argument passing styles predominate: `T` --
        you own this one, and `const T&` -- this is input.  Any mutable
        references, like `T&`, ought to be plainly visible at the callsite
        to avoid bugs, but unfortunately C++ doesn't support this.
        `Bindings.h` remedies this design flaw.

XXX revise

The "binding" structures in this file model C++ variable assignment.  A
binding's lifetime should be limited to a single statement -- it is merely
plumbing that maps a user-supplied value to some storage. It has 3 pieces:
  - An identifier tag type (attached via `Identifier::operator=`). The UX is
    simply `"x"_id = ...` or `self = ...`.
  - A reference (`RefBinding`) or value (`InPlaceBinding`) being bound to the
    identifier. The RHS of `operator=` can be:
      - `lval`: the binding stores an lvalue ref internally, or
      - `std::move(lval)` or prvalue: the binding stores an rvalue ref, or
      - A "projection wrapper" around one of the above, see next bullet.
  - `Projection` maps the bound ref/value to a storage type (`StorageType` /
    `SignatureType`).  Unlike vanilla C++, and in the spirit of pure
    functional languages, the default projection stores const values.  You
    can `asNonConst` or `asRef` groups of bindings, as needed.

Note that the "binding modifiers" of `asConst`, `asNonConst`, `asRef`,
`asNonConstRef` may be used in many contexts (tests have more demos):

  "x"_id = asNonConst(5)
  asNonConst("y" = b)
  asRef(self = inPlace<Foo>(), asNonConst("x" = 1, "y" = 2))

This is intended to support concise specification of bindings, including
binding all, or part, of a lexical scope into another scope.

Bindings also have concisely written storage signatures:
  SelfType<const int>
  IdType<"x", int&&>
*/

namespace folly::bindings {

template <typename, typename... As>
constexpr auto make_in_place(As&&...);
constexpr auto make_in_place_via(auto);

namespace detail {

enum class category_projection {
  unset = 0, // The binding policy decides. The standard policy uses "VALUE".
  ref, // Follows reference category of the input, i.e. `InputT&&`.
  value, // Copies or moves, depending on input ref type.
  copy, // Like `value`, but fails on rvalue input.
  move // Like `value`, but fails on lvalue input.
};

enum class const_projection {
  // The binding policy decides.  The standard policy uses `const` for refs,
  // `non_const` for values.
  unset = 0,
  // For reference types, these both affect the underlying value type:
  constant, // Make the input `const` if it's not already.
  non_constant // Will NOT remove `const` from an input type
};

struct bind_info {
  category_projection category_p;
  const_projection const_p;

  explicit bind_info() = default;
  constexpr explicit bind_info(category_projection ct, const_projection cn)
      : category_p(ct), const_p(cn) {}
};

template <typename T>
concept is_bind_info = std::is_convertible_v<T*, bind_info*>;

template <typename T>
concept can_update_bind_info = requires(T b) {
  requires is_bind_info<
      std::remove_cvref_t<decltype(std::remove_cvref_t<T>::bind_info)>>;
  std::move(b).template update_bind_info<bind_info{}>();
};

constexpr auto update_bind_info(can_update_bind_info auto b, auto fn) {
  FOLLY_PUSH_WARNING
  // `lifetimebound` analysis is too coarse, and doesn't have a knob to
  // connect the lifetime of the return value to that of `b`.  This kind of
  // scenario shouldn't come up in user code, so the annotation adds safety.
  FOLLY_GNU_DISABLE_WARNING("-Wreturn-stack-address")
  return std::move(b).template update_bind_info<fn(b.bind_info)>();
  FOLLY_POP_WARNING
}

// Bindings may contain r-value refs; move-only prevents double use.
template <is_bind_info auto BI, typename T>
class ref_binding : private MoveOnly {
  // `&&` allows binding R-values. Safe, since a binding lives for 1 statement.
  T&& ref_;

 protected:
  friend constexpr auto ensure_binding(auto&& v);
  template <is_bind_info auto, typename>
  friend class ref_binding;

  // NB: The forwarding here isn't doing much. `ensure_binding` deduces `T`.
  constexpr explicit ref_binding(T&& t [[clang::lifetimebound]])
    requires(!can_update_bind_info<T>) // This would be an unexpected usage.
      : ref_(std::forward<T>(t)) {}

 public:
  // Too conservative, as per the `lifetimebound` note in `in_place_binding`
  constexpr decltype(auto) what_to_bind() && [[clang::lifetimebound]] {
    return std::forward<T>(ref_);
  }

  template <is_bind_info auto NewBI>
  constexpr auto update_bind_info() && [[clang::lifetimebound]] {
    return ref_binding<NewBI, T>{std::forward<T>(ref_)};
  }

  static constexpr auto bind_info = BI;
  using binding_type = T; // Can be a value or a reference type
};

// If the arg is already a binding, returns that unmodified binding.
// Otherwise, make a `ref_binding` that preserves the value category.
constexpr auto ensure_binding(auto&& v) {
  if constexpr (can_update_bind_info<decltype(v)>) {
    static_assert(!std::is_lvalue_reference_v<decltype(v)>);
    return std::move(v);
  } else {
    return ref_binding<bind_info{}, decltype(v)>{std::forward<decltype(v)>(v)};
  }
}

template <typename Fn>
struct in_place_fn_wrap {
  using make_fn_type = Fn;
  make_fn_type make_fn_;
};

template <typename T, typename FnWrap>
class in_place_fn_maker {
 private:
  // May contain refs, but that's safe since a binding lives for 1 statement.
  FnWrap fn_wrap_;

  template <is_bind_info auto, typename, typename...>
  friend class in_place_binding;

  constexpr explicit in_place_fn_maker(FnWrap fw) : fn_wrap_(std::move(fw)) {}

 public:
  // This implicit conversion allows direct construction inside of `tuple` e.g.
  constexpr /*implicit*/ operator T() && { return fn_wrap_.make_fn_(); }
};

template <typename T, typename... Args>
class in_place_args_maker {
 private:
  // `&&` allows binding R-values. Safe, since a binding lives for 1 statement.
  std::tuple<Args&&...> arg_tup_;

  template <is_bind_info auto, typename, typename...>
  friend class in_place_binding;

  constexpr explicit in_place_args_maker(Args&&... as [[clang::lifetimebound]])
      : arg_tup_(std::forward<Args>(as)...) {}

 public:
  // This implicit conversion allows direct construction inside of `tuple` e.g.
  constexpr /*implicit*/ operator T() && {
    return std::make_from_tuple<T>(std::move(arg_tup_));
  }

  // Power users may want to rewrite the args of an in-place binding.
  constexpr auto&& release_arg_tuple() && { return std::move(arg_tup_); }
};

// Bindings may contain r-value refs; move-only prevents double use.
template <is_bind_info auto BI, typename T, typename... Args>
class in_place_binding : private MoveOnly {
 private:
  static_assert(!std::is_reference_v<T>);
  static_assert(
      BI.category_p != category_projection::ref,
      "`by_ref` is incompatible with `make_in_place*`");

  static constexpr bool use_fn_maker =
      (sizeof...(Args) == 1 &&
       (is_instantiation_of_v<in_place_fn_wrap, Args> && ...));

  using maker_type = typename decltype([]() {
    if constexpr (use_fn_maker) {
      return std::type_identity<in_place_fn_maker<T, Args...>>{};
    } else {
      return std::type_identity<in_place_args_maker<T, Args...>>{};
    }
  }())::type;

  maker_type maker_;

  template <is_bind_info auto, typename, typename...>
  friend class in_place_binding;

  constexpr explicit in_place_binding(maker_type m) : maker_(std::move(m)) {}

 protected:
  template <typename, typename... As>
  friend constexpr auto ::folly::bindings::make_in_place(As&&...);
  friend constexpr auto ::folly::bindings::make_in_place_via(auto);

  // NB: `Args` are deduced by `make_in_place` or `make_in_place_via`.
  constexpr explicit in_place_binding(
      std::in_place_t,
      Args&&... args [[clang::lifetimebound]])
    requires(!can_update_bind_info<T>) // This would be an unexpected usage.
      : maker_{std::forward<Args>(args)...} {}

 public:
  using type = T;

  // In both functions below, `lifetimebound` is technically overly
  // conservative, because we hand ownership of the refs in `maker_` to the
  // caller.  However, it's still desirable because I don't know of a more
  // direct / precise way to tell the compiler that this object must not
  // outlive any of the refs in `in_place_binding` above.  For an explicit
  // example, if you removed this `lifetimebound`, the compiler could no
  // longer catch this dangling ref in runtime code (`Foo` defined in test):
  //   auto fooMaker = make_in_place<Foo>(&made, n).what_to_bind();
  //   Foo foo = std::move(fooMaker);

  // To allow in-place construction within a `tuple<..., T, ...>`, this
  // returns a `Maker` that's implicitly convertible to `T`.
  constexpr auto what_to_bind() && [[clang::lifetimebound]] {
    return std::move(maker_);
  }

  template <is_bind_info auto NewBI>
  constexpr auto update_bind_info() && [[clang::lifetimebound]] {
    return in_place_binding<NewBI, T, Args...>{std::move(maker_)};
  }

  static constexpr auto bind_info = BI;
  using binding_type = T; // Must be a value type (per `in_place_maker`)
};

template <typename>
inline constexpr bool is_in_place_binding_v = false;
template <auto BI, typename T, typename... Args>
inline constexpr bool is_in_place_binding_v<in_place_binding<BI, T, Args...>> =
    true;

template <auto, typename, typename = void>
class binding_policy_impl;

// This is a separate class so that libraries can customize the binding
// policy enacted by `binding_policy` by detecting their custom fields in
// `BI`.  See `named/Binding.h` for an example.  IMPORTANT:
// XXX slice the object!
//  - DANGER: To avoid UB, your specialization must match your EXACT key set
//    via `type_idx_map_of<binding_keys_plus_<YOUR_KEYS>>`.
//  - The prior bullet means you must define a separate specialization to
//    handle each combination of keys.
//  - DO delegate handling of `binding_keys` to this specialization, by
//    composition.  Any deviations from this standard policy are likely to
//    confuse users & readers of your library, and are probably not worth
//    it.  If you REALLY need to deviate (ex: default bindings to `const`),
//    make the name of your API reflect this (ex: `fooDefaultConst()`).
template <bind_info BI, can_update_bind_info Binding>
class binding_policy_impl<BI, Binding> {
  // XXX needed?  static_assert(BI ==
  // Binding::bind_info.copy_some(binding_keys));

 protected:
  template <typename T>
  using add_const_inside_ref = std::conditional_t<
      std::is_rvalue_reference_v<T>,
      typename std::add_const<std::remove_reference_t<T>>::type&&,
      std::conditional_t<
          std::is_lvalue_reference_v<T>,
          typename std::add_const<std::remove_reference_t<T>>::type&,
          typename std::add_const<std::remove_reference_t<T>>::type>>;

  template <typename RawT> // can be ref or value
  constexpr static auto project_type() {
    constexpr auto cat_p = BI.category_p;
    if constexpr (cat_p == category_projection::ref) {
      // By-reference: `const` by default
      if constexpr (BI.const_p == const_projection::non_constant) {
        return std::type_identity<RawT&&>{}; // Leave pre-existing `const`!
      } else {
        return std::type_identity<add_const_inside_ref<RawT>&&>{};
      }
    } else {
      if constexpr (cat_p == category_projection::copy) {
        static_assert(!std::is_rvalue_reference_v<RawT>);
      } else if constexpr (cat_p == category_projection::move) {
        static_assert(std::is_rvalue_reference_v<RawT>);
      } else {
        static_assert(
            (cat_p == category_projection::value) ||
            (cat_p == category_projection::unset));
      }
      // By-value: non-`const` by default
      using UnrefT = std::remove_reference_t<RawT>;
      if constexpr (BI.const_p == const_projection::constant) {
        return std::type_identity<const UnrefT>{};
      } else {
        return std::type_identity<UnrefT>{};
      }
    }
  }

 public:
  using storage_type =
      typename decltype(project_type<typename Binding::binding_type>())::type;
  using signature_type = storage_type;
};

template <can_update_bind_info Binding>
class binding_policy : public binding_policy_impl<Binding::bind_info, Binding> {
  // Future: It may be valid to relax this.  The current rationale is that
  // if you change the underlying `storage_type`, then you'll have trouble
  // storing `binding.what_to_bind()` in it.
#if 0 // XXX
  static_assert(
      std::is_same_v<
          std::remove_cvref_t<typename binding_policy_impl<
              Binding::bind_info,
              Binding>::storage_type>,
          std::remove_cvref_t<typename binding_policy_impl<
              Binding::bind_info.copy_some(binding_keys),
              Binding>::storage_type>>,
      "binding_policy specializations shouldn't change storage_type");
#endif
};

// 0-dep, 5-line `folly::overload` without `std::variant` integration. From:
// https://www.scs.stanford.edu/~dm/blog/param-pack.html#multilambda
template <typename... L>
struct multilambda : L... {
  using L::operator()...;
  constexpr multilambda(L... lambda) : L(std::move(lambda))... {}
};

// `decls` is either something that `fn` can convert to a binding, or a a
// binding tuple (these may be nested).
constexpr auto map_flatten_bindings(
    auto fn, auto&&... decls [[clang::lifetimebound]]) {
  return std::tuple_cat(multilambda(
      // Perfect-forward single bindings, so that `ensure_binding_tuple`
      // can preserve value category through `ensure_binding`.
      [&]<typename B>(B&& b) { return std::tuple{fn(std::forward<B>(b))}; },
      // In our usage, all binding tuples are ephemeral.
      [&]<typename... Bs>(std::tuple<Bs...>&& bTup)
        requires(
            (detail::can_update_bind_info<Bs> && !std::is_reference_v<Bs>) &&
            ...)
      {
        return std::apply(
            [&](auto&&... bs) {
              return map_flatten_bindings(fn, std::move(bs)...);
            },
            std::move(bTup));
      })(std::forward<decltype(decls)>(decls))...);
}

template <auto BindInfoFn = std::identity{}>
struct ensure_binding_tuple_with_ {
  constexpr auto operator()(auto&&... decls) const {
    // We need the function to run **before** constructing the `tuple`,
    // or it's hard to preserve the value category.
    return map_flatten_bindings(
        [](auto&& b) {
          return update_bind_info(
              ensure_binding(std::forward<decltype(b)>(b)), BindInfoFn);
        },
        std::forward<decltype(decls)>(decls)...);
  }
};

} // namespace detail

// `make_in_place` and `make_in_place_via` construct non-movable,
// non-copyable types in their final location.
template <typename T, typename... Args>
constexpr auto make_in_place(Args&&... args [[clang::lifetimebound]]) {
  return detail::in_place_binding<detail::bind_info{}, T, Args...>{
      std::in_place, std::forward<Args>(args)...};
}
// This is second-choice compared to `make_in_place` because:
//   - Refs are hidden inside `make_fn`, so `lifetimebound` can't
//     detect dangling references for you here.
//   - The type signature of the `in_place_binding` includes a lambda.
constexpr auto make_in_place_via(auto make_fn) {
  detail::in_place_fn_wrap<decltype(make_fn)> fn_wrap{
      .make_fn_ = std::move(make_fn)};
  FOLLY_PUSH_WARNING
  // The warning bypass is here because `lifetimebound` on the
  // `in_place_binding` constructor is too conservative.  The annotation is
  // great for `make_in_place`, which typically feeds references into the
  // binding, but in this case, we're actually moving `fn_wrap` into the
  // binding.  If desired, a separate ctor could make this unnecessary.
  FOLLY_GNU_DISABLE_WARNING("-Wreturn-stack-address")
  return detail::in_place_binding<
      detail::bind_info{},
      std::invoke_result_t<decltype(make_fn)>,
      decltype(fn_wrap)>{std::in_place, std::move(fn_wrap)};
  FOLLY_POP_WARNING
}

// Unlike standard C++, `constant` commutes with `by_ref`, no footgun here.
inline constexpr detail::ensure_binding_tuple_with_<[](auto bi) {
  bi.const_p = detail::const_projection::constant;
  return bi;
}>
    constant;
inline constexpr detail::ensure_binding_tuple_with_<[](auto bi) {
  bi.const_p = detail::const_projection::non_constant;
  return bi;
}>
    non_constant;
inline constexpr detail::ensure_binding_tuple_with_<[](auto bi) {
  bi.category_p = detail::category_projection::ref;
  return bi;
}>
    by_ref;
inline constexpr detail::ensure_binding_tuple_with_<[](auto bi) {
  bi.category_p = detail::category_projection::ref;
  bi.const_p = detail::const_projection::non_constant;
  return bi;
}>
    by_non_const_ref;
// Future: Add `copied()` and `moved()` modifiers so the user can ensure
// pass-by-value with copy-, or move-copy semantics.  This enforcement
// already exists in `binding_policy_impl` and `category_projection`.

// To write a `Bindings.h`-enabled function, just perfect-forward your args
// into this helper, and then work on the resulting bindings.
//
// BEWARE: Since binding modifiers can apply to (and therefore emit)
// multiple bindings, the number of elements in the resulting `tuple` may
// differ from `sizeof...(YourArgs)`.  Only look at the `tuple`.
inline constexpr detail::ensure_binding_tuple_with_<std::identity{}>
    ensure_binding_tuple;

} // namespace folly::bindings
