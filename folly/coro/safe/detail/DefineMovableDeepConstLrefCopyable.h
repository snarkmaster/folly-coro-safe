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

// DELIBERATELY omits `#pragma once`

#include <type_traits>

// IMPORTANT:
//   - `#include` just before use.
//   - `#undef` after using, don't leak the macro from your header!
//
// Usage:
//   #include "DefineMovableDeepConstLrefCopyable.h"
//   template <typename T> // either ref or value
//   class YourClass {
//    public:
//     FOLLY_MOVABLE_AND_DEEP_CONST_LREF_COPYABLE(YourClass, T);
//     // Don't leak the macro from your header!
//     #undef FOLLY_MOVABLE_AND_DEEP_CONST_LREF_COPYABLE
//   };
//
// Similar to `folly::MoveOnly`, plus some copyability if `T` is an lval ref:
//   - `YourClass<const V&>` is fully copyable
//   - For non-const `V`, you can copy from `YourClass<V&>&` but NOT from
//     `const YourClass<V&>&`.
// In other words, `YourClass<const int&>` is fully copyable, but
// `YourClass<int&>` is only copyable from a non-const ref.
//
// The design intent is that `YourClass<T>` behaves like a move-only `T`,
// and, if so, l-value references should be copyable -- with a caveat.
// "Deep const" means that copying `const YourClass<V&>` should NOT be able
// to drop the `const` qualification. The restriction copy ctor ensures this.
//
// Notes:
//   - Unfortunately, this HAS to be a macro, applied on the leaf class in
//     your inheritance hierarchy.  This requirement comes about because if
//     you derive from a class with this macro, the C++ default-constructor
//     machinery treats the base class as non-copyable, instead of the more
//     nuanced behavior we need.
//   - This lacks restrictions on a defaulted move from `const classname&&`
//     because this would only apply when copyable, and our copy constructor
//     already enforces the "deep const" behavior.
#define FOLLY_MOVABLE_AND_DEEP_CONST_LREF_COPYABLE(class_name, inner_type) \
  class_name(class_name&&) noexcept(                                       \
      std::is_nothrow_move_constructible_v<inner_type>) = default;         \
  class_name& operator=(class_name&&) noexcept(                            \
      std::is_nothrow_move_assignable_v<inner_type>) = default;            \
  class_name(class_name&) noexcept(                                        \
      std::is_nothrow_copy_constructible_v<inner_type>)                    \
    requires std::is_lvalue_reference_v<inner_type>                        \
  = default;                                                               \
  class_name& operator=(class_name&) noexcept(                             \
      std::is_nothrow_copy_assignable_v<inner_type>)                       \
    requires std::is_lvalue_reference_v<inner_type>                        \
  = default;                                                               \
  class_name(const class_name&) noexcept(                                  \
      std::is_nothrow_copy_constructible_v<inner_type>)                    \
    requires(std::is_lvalue_reference_v<inner_type> &&                     \
             std::is_const_v<std::remove_reference_t<inner_type>>)         \
  = default;                                                               \
  class_name& operator=(const class_name&) noexcept(                       \
      std::is_nothrow_copy_assignable_v<inner_type>)                       \
    requires(std::is_lvalue_reference_v<inner_type> &&                     \
             std::is_const_v<std::remove_reference_t<inner_type>>)         \
  = default
