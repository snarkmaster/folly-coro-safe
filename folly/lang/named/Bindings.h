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

#include <tuple>
#include <type_traits>

#include <folly/Traits.h>
#include <folly/Utility.h>
#include <folly/lang/Bindings.h>

#ifndef _WIN32 // Explained in folly/lang/Bindings.h

/*
XXX first work on async closure bindings

Then do named/Scopes.h

named::Values
named::UnsafeArgs
named::Named or Scope???
*/

///
/// Extends `lang/Bindings.h` (read that first) with keyword-argument syntax:
///   self = make_in_place<MyClass>(), "x"_id = 5, "y"_id = by_non_const_ref(y)
///
/// If the API takes a pack of named bindings, the above syntax is all that
/// a callers needs to know.  Some API mays require you to wrap the pack
/// with a class like `named::Args{...}` or `named::Values{...}`.
///
/// Library authors can use this to support kwargs in their functions:
///   - XXX use either `ensure_binding_tuple` or named/Scope.h
///   - Take 1 arg constrained to `instantiated_from<named::Values> kwargs`
///     (or `named::UnsafeArgs` if you must take references) and use it to
///     construct a `named::Scope`.
///

namespace folly::named {

namespace detail {

template <typename IdTag, typename Base>
struct named_bind_info : Base {
  using Base::Base;
  constexpr explicit named_bind_info(Base b) : Base(std::move(b)) {}
};

template <typename Tag>
struct identifier {
  // "x"_id = non_constant(var)
  template <typename B>
    requires ::folly::bindings::detail::can_update_bind_info<B>
  constexpr auto operator=(std::tuple<B> bt) const {
    return ::folly::bindings::detail::update_bind_info(
        ::folly::bindings::detail::ensure_binding(std::get<0>(std::move(bt))),
        [](std::derived_from<folly::bindings::detail::bind_info> auto bi) {
          return named_bind_info<Tag, decltype(bi)>{std::move(bi)};
        });
  }
  // "x"_id = var
  template <typename T>
  constexpr auto operator=(T&& t [[clang::lifetimebound]]) const {
    return ::folly::bindings::detail::update_bind_info(
        ::folly::bindings::detail::ensure_binding(std::forward<T>(t)),
        [](std::derived_from<folly::bindings::detail::bind_info> auto bi) {
          return named_bind_info<Tag, decltype(bi)>{std::move(bi)};
        });
  }
};

struct self_t {}; // Tag for `self` identifier

} // namespace detail

// These types represents "identifier tag -> storage type" signatures for
// bindings. Library authors: search for `signature_type`.
//
// NB: `id_type` is distinct from `self_type` for cleaner error messages.
// Logically, both of these are `tag_type` (or similar), and the
// implementation could later be generalized, but for now, this is ok.
template <typename T>
struct self_type {
  using identifier_type = detail::identifier<detail::self_t>;
  using storage_type = T;
};
template <literal_string Str, typename T>
struct id_type {
  using identifier_type = detail::identifier<vtag_t<Str>>;
  using storage_type = T;
};

// For brevity, `identifier`s are usually made via custom string literals.
inline namespace literals {
inline namespace string_literals {
template <literal_string Str>
consteval auto operator""_id() noexcept {
  return detail::identifier<vtag_t<Str>>{};
}
} // namespace string_literals
} // namespace literals

// A special `identifier` that enables syntax sugar for calling class members.
inline constexpr detail::identifier<detail::self_t> self;

} // namespace folly::named

// Customize the standard binding storage/signature policy
namespace folly::bindings::detail {
template <
    typename Tag,
    typename BIBase,
    folly::named::detail::named_bind_info<Tag, BIBase> BI,
    can_update_bind_info Binding>
class binding_policy_impl<BI, Binding> {
 private:
  using standard = binding_policy_impl<bind_info{BI}, Binding>;

  static constexpr auto signature_type_for(::folly::named::detail::self_t) {
    return std::type_identity<::folly::named::self_type<storage_type>>{};
  }
  template <literal_string Str>
  static constexpr auto signature_type_for(vtag_t<Str>) {
    return std::type_identity<::folly::named::id_type<Str, storage_type>>{};
  }

 public:
  using storage_type = typename standard::storage_type;
  using signature_type = typename decltype(signature_type_for(Tag{}))::type;
};
} // namespace folly::bindings::detail

#endif
