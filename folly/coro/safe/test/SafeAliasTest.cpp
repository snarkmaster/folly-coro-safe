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

#include <folly/coro/safe/SafeAlias.h>
#include <folly/portability/GTest.h>

using namespace folly;

using SA = safe_alias;

TEST(SafeAliasTest, basics) {
  class X {};
  static_assert(safe_alias_of_v<X> == SA::maybe_value);
  static_assert(safe_alias_of_v<int> == SA::maybe_value);
  static_assert(safe_alias_of_v<int*> == SA::unsafe);
  static_assert(safe_alias_of_v<const int*> == SA::unsafe);
  static_assert(safe_alias_of_v<int&> == SA::unsafe);
  static_assert(safe_alias_of_v<const int&&> == SA::unsafe);
  static_assert(safe_alias_of_v<std::reference_wrapper<int>> == SA::unsafe);
  static_assert(
      safe_alias_of_v<const std::reference_wrapper<int>> == SA::unsafe);
  static_assert(
      safe_alias_of_v<const std::reference_wrapper<int>&> == SA::unsafe);
  static_assert(
      safe_alias_of_v<folly::rvalue_reference_wrapper<X>> == SA::unsafe);
}

TEST(SafeAliasTest, tuple) {
  class X {};
  static_assert(safe_alias_of_v<std::tuple<X, int>> == SA::maybe_value);
  static_assert(safe_alias_of_v<std::tuple<X&, int>> == SA::unsafe);
  static_assert(safe_alias_of_v<std::tuple<X, int*>> == SA::unsafe);
}

TEST(SafeAliasTest, safety_ordering) {
  class X {};
  static_assert(safe_alias_of_v<std::tuple<X, int>> == SA::maybe_value);
  static_assert(
      safe_alias_of_v<
          std::
              tuple<X, int, manual_safe_ref_t<SA::co_cleanup_safe_ref, int>>> ==
      SA::co_cleanup_safe_ref);
  static_assert(
      safe_alias_of_v<std::tuple<
          X,
          int,
          manual_safe_ref_t<SA::body_only_ref, int>,
          manual_safe_ref_t<SA::co_cleanup_safe_ref, int>>> ==
      SA::body_only_ref);
  static_assert(
      safe_alias_of_v<std::tuple<
          int,
          manual_safe_ref_t<SA::body_only_ref, int>,
          manual_safe_ref_t<SA::shared_cleanup, int>>> == SA::shared_cleanup);
  static_assert(
      safe_alias_of_v<std::tuple<
          X,
          int,
          manual_safe_ref_t<SA::body_only_ref, bool>,
          int&>> == SA::unsafe);
}

TEST(SafeAliasTest, pair) {
  class X {};
  static_assert(safe_alias_of_v<std::pair<X, int>> == SA::maybe_value);
  static_assert(safe_alias_of_v<std::pair<X&, int>> == SA::unsafe);
  static_assert(safe_alias_of_v<std::pair<X, int*>> == SA::unsafe);
}

TEST(SafeAliasTest, vector) {
  class X {};
  static_assert(safe_alias_of_v<std::vector<X, int>> == SA::maybe_value);
  static_assert(safe_alias_of_v<std::vector<int>> == SA::maybe_value);
  static_assert(safe_alias_of_v<std::vector<X&>> == SA::unsafe);
  static_assert(safe_alias_of_v<std::vector<int*>> == SA::unsafe);
}

TEST(SafeAliasTest, manual_safe_ref) {
  int* x = nullptr;
  auto r1 = manual_safe_ref(x);
  auto r2 = manual_safe_ref<safe_alias::co_cleanup_safe_ref>(x);
  auto r3 = manual_safe_ref<safe_alias::body_only_ref>(x);
  static_assert(safe_alias_of_v<decltype(x)> == SA::unsafe);
  static_assert(safe_alias_of_v<decltype(r1)> == SA::maybe_value);
  static_assert(safe_alias_of_v<decltype(r2)> == SA::co_cleanup_safe_ref);
  static_assert(safe_alias_of_v<decltype(r3)> == SA::body_only_ref);
}

TEST(SafeAliasTest, manual_safe_val) {
  int* x = nullptr;
  auto r1 = manual_safe_val(x);
  auto r2 = manual_safe_val<safe_alias::co_cleanup_safe_ref>(x);
  auto r3 = manual_safe_val<safe_alias::body_only_ref>(x);
  static_assert(safe_alias_of_v<decltype(x)> == SA::unsafe);
  static_assert(safe_alias_of_v<decltype(r1)> == SA::maybe_value);
  static_assert(safe_alias_of_v<decltype(r2)> == SA::co_cleanup_safe_ref);
  static_assert(safe_alias_of_v<decltype(r3)> == SA::body_only_ref);
}
