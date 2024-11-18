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

#include <folly/lang/named/Bindings.h>
#include <folly/portability/GTest.h>

//
// IMPORTANT: This is intended to parallel `folly/lang/test/BindingsTest.cpp`!
// To reduce redundancy, we don't repeat some of the tests here.
//

using namespace folly;
using namespace folly::bindings;
using namespace folly::bindings::detail;
using namespace folly::named;
using namespace folly::named::detail;

template <typename T>
using twice = std::pair<T, T>;

// This is here so that test "runs" show up in CI history
TEST(NamedBindingsTest, all_tests_run_at_build_time) {}

// Better UX than `assert()` in constexpr tests.
constexpr void test(bool ok) {
  if (!ok) {
    throw std::exception(); // Throwing in constexpr code is a compile error
  }
}

template <
    literal_string Id,
    const_projection Const = const_projection{},
    category_projection Cat = category_projection{}>
inline constexpr named_bind_info<vtag_t<Id>, bind_info> named_bi{Cat, Const};
template <
    const_projection Const = const_projection{},
    category_projection Cat = category_projection{}>
inline constexpr named_bind_info<self_t, bind_info> self_bi{Cat, Const};

constexpr auto check_ref_binding() {
  static_assert(
      std::is_same_v<decltype("x"_id = 5), ref_binding<named_bi<"x">, int&&>>);

  int y = 5;
  {
    auto lval = (self = y);
    static_assert(std::is_same_v<decltype(lval), ref_binding<self_bi<>, int&>>);
    y += 20;
    test(25 == std::move(lval).what_to_bind());
  }

  {
    auto rval = ("y"_id = std::move(y));
    static_assert(
        std::is_same_v<decltype(rval), ref_binding<named_bi<"y">, int&&>>);
    y -= 10;
    test(15 == std::move(rval).what_to_bind());
  }

  return true;
}

static_assert(check_ref_binding());

constexpr auto check_flatten_bindings() {
  int b = 2, d = 4;
  using FlatT = decltype(ensure_binding_tuple(
      "a"_id = 1.2,
      std::tuple{"b"_id = b, "c"_id = 'x'},
      "d"_id = d,
      std::tuple{},
      "e"_id = "abc"));
  static_assert(std::is_same_v<
                FlatT,
                std::tuple<
                    ref_binding<named_bi<"a">, double&&>,
                    ref_binding<named_bi<"b">, int&>,
                    ref_binding<named_bi<"c">, char&&>,
                    ref_binding<named_bi<"d">, int&>,
                    ref_binding<named_bi<"e">, const char(&)[4]>>>);
  return true;
}

static_assert(check_flatten_bindings());

constexpr auto check_as_const_and_non_const() {
  double b = 2.3;

  auto bs = non_constant("a"_id = 1, std::tuple{"b"_id = b, "c"_id = 'c'});
  constexpr auto non_const = const_projection::non_constant;
  static_assert(std::is_same_v<
                decltype(bs),
                std::tuple<
                    ref_binding<named_bi<"a", non_const>, int&&>,
                    ref_binding<named_bi<"b", non_const>, double&>,
                    ref_binding<named_bi<"c", non_const>, char&&>>>);

  auto cbs = constant(std::move(bs));
  constexpr auto konst = const_projection::constant;
  static_assert(std::is_same_v<
                decltype(cbs),
                std::tuple<
                    ref_binding<named_bi<"a", konst>, int&&>,
                    ref_binding<named_bi<"b", konst>, double&>,
                    ref_binding<named_bi<"c", konst>, char&&>>>);

  return true;
}

static_assert(check_as_const_and_non_const());

constexpr auto check_by_ref() {
  double b = 2.3;

  auto rbs = by_ref("a"_id = 1, std::tuple{"b"_id = b, "c"_id = 'c'});
  constexpr auto ref = category_projection::ref;
  constexpr const_projection def_const{};
  static_assert(std::is_same_v<
                decltype(rbs),
                std::tuple<
                    ref_binding<named_bi<"a", def_const, ref>, int&&>,
                    ref_binding<named_bi<"b", def_const, ref>, double&>,
                    ref_binding<named_bi<"c", def_const, ref>, char&&>>>);

  constexpr auto non_const = const_projection::non_constant;
  using non_const_refs = std::tuple<
      ref_binding<named_bi<"a", non_const, ref>, int&&>,
      ref_binding<named_bi<"b", non_const, ref>, double&>,
      ref_binding<named_bi<"c", non_const, ref>, char&&>>;
  auto bs1 = non_constant(std::move(rbs));
  static_assert(std::is_same_v<decltype(bs1), non_const_refs>);

  auto bs2 = by_non_const_ref("a"_id = 1, std::tuple{"b"_id = b, "c"_id = 'c'});
  static_assert(std::is_same_v<decltype(bs2), non_const_refs>);

  return true;
}

static_assert(check_by_ref());

struct Foo : folly::NonCopyableNonMovable {
  constexpr explicit Foo(bool* made, int n) : n_(n) {
    if (made) {
      *made = true;
    }
  }
  int n_;
};

// NB: These signatures are NOT meant to be user-visible.
constexpr auto check_in_place_binding_type_sig() {
  static_assert(std::is_same_v<
                decltype("x"_id = make_in_place<Foo>(nullptr, 7)),
                in_place_binding<named_bi<"x">, Foo, std::nullptr_t, int>>);
  // Composes with projection modifiers as expected
  static_assert(std::is_same_v<
                decltype("x"_id = constant(make_in_place<Foo>(nullptr, 7))),
                in_place_binding<
                    named_bi<"x", const_projection::constant>,
                    Foo,
                    std::nullptr_t,
                    int>>);
  return true;
}

static_assert(check_in_place_binding_type_sig());

constexpr auto check_in_place_binding_natural_usage() {
  // projections don't affect `what_to_bind`, just the storage type
  Foo f1 = ("x"_id = constant(make_in_place<Foo>(nullptr, 17))).what_to_bind();
  test(17 == f1.n_);

  int n = 3;
  Foo f2 = ("y"_id = make_in_place<Foo>(nullptr, n)).what_to_bind();
  ++n;
  test(3 == f2.n_);
  test(4 == n);

  return true;
}

static_assert(check_in_place_binding_natural_usage());

constexpr auto check_in_place_binding_modifier_distributive_property() {
  constexpr auto ref = category_projection::ref;
  constexpr auto non_const = const_projection::non_constant;
  using my_list = std::tuple<
      ref_binding<named_bi<"a", non_const>, bool&&>,
      ref_binding<named_bi<"b", non_const, ref>, double&>,
      in_place_binding<named_bi<"c", non_const>, int, int>,
      ref_binding<self_bi<non_const, ref>, char&&>>;

  double b = 2;
  static_assert(std::is_same_v<
                my_list,
                decltype(non_constant(
                    "a"_id = true,
                    "b"_id = by_ref(b),
                    "c"_id = make_in_place<int>(3),
                    self = by_ref('d')))>);
  static_assert(std::is_same_v<
                my_list,
                decltype(non_constant(
                    "a"_id = non_constant(true),
                    "b"_id = by_non_const_ref(b),
                    "c"_id = non_constant(make_in_place<int>(3)),
                    self = by_non_const_ref('d')))>);
  static_assert(std::is_same_v<
                my_list,
                decltype(non_constant(
                    non_constant("a"_id = true),
                    by_non_const_ref("b"_id = b),
                    non_constant("c"_id = make_in_place<int>(3)),
                    by_non_const_ref(self = 'd')))>);

  return true;
}

static_assert(check_in_place_binding_modifier_distributive_property());

// A minimal test that `storage_type` matches standard policy
static_assert(std::is_same_v<
              typename binding_policy<
                  std::tuple_element_t<0, decltype(constant(by_ref(5)))>>::
                  storage_type,
              const int&&>);

template <can_update_bind_info B>
using sig = typename binding_policy<B>::signature_type;

constexpr auto check_in_place_binding_signature_type() {
  static_assert(std::is_same_v<
                sig<decltype("x"_id = constant(5))>,
                id_type<"x", const int>>);
  static_assert(std::is_same_v<
                sig<decltype("x"_id = non_constant(5))>,
                id_type<"x", int>>);
  static_assert(std::is_same_v<
                sig<decltype("x"_id = by_non_const_ref(5))>,
                id_type<"x", int&&>>);
  static_assert(std::is_same_v<
                sig<decltype(self = constant(make_in_place<int>(5)))>,
                self_type<const int>>);

  return true;
}

static_assert(check_in_place_binding_signature_type());
