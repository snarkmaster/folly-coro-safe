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

#include <folly/lang/Bindings.h>
#include <folly/portability/GTest.h>

using namespace folly::bindings;
using namespace folly::bindings::detail;

template <typename T>
using twice = std::pair<T, T>;

struct Foo : folly::NonCopyableNonMovable {
  constexpr explicit Foo(bool* made, int n) : n_(n) {
    if (made) {
      *made = true;
    }
  }
  int n_;
};

// This is here so that test "runs" show up in CI history
TEST(BindingsTest, all_tests_run_at_build_time) {
  // This is a manually-enabled example of the `lifetimebound` annotation on
  // `in_place_binding::what_to_bind()`.  With `lifetimebound` it won't
  // compile, without it would hit an ASAN failure.  It has to be a runtime
  // test because `constexpr` evaluation detects usage of dangling
  // references regardless of `lifetimebound`..
#if 0
  int n = 1337;
  bool made = false;
  auto fooMaker = make_in_place<Foo>(&made, n).what_to_bind();
  Foo foo = std::move(fooMaker);
  EXPECT_TRUE(made);
  EXPECT_EQ(1337, foo.n_);
  FAIL() << "Should not compile, or at least fail under ASAN.";
#endif
}

// Better UX than `assert()` in constexpr tests.
constexpr void test(bool ok) {
  if (!ok) {
    throw std::exception(); // Throwing in constexpr code is a compile error
  }
}

constexpr auto check_ref_binding() {
  static_assert(std::is_same_v<
                decltype(ensure_binding(5)),
                ref_binding<bind_info{}, int&&>>);

  int y = 5;
  {
    auto lval = ensure_binding(y);
    static_assert(
        std::is_same_v<decltype(lval), ref_binding<bind_info{}, int&>>);
    y += 20;
    test(25 == std::move(lval).what_to_bind());
  }

  {
    auto rval = ensure_binding(std::move(y));
    static_assert(
        std::is_same_v<decltype(rval), ref_binding<bind_info{}, int&&>>);
    y -= 10;
    test(15 == std::move(rval).what_to_bind());
  }

  return true;
}

static_assert(check_ref_binding());

constexpr auto check_flatten_bindings() {
  int b = 2, d = 4;
  using FlatT = decltype(ensure_binding_tuple(
      1.2, ensure_binding_tuple(b, 'x'), d, std::tuple{}, "abc"));
  static_assert(std::is_same_v<
                FlatT,
                std::tuple<
                    ref_binding<bind_info{}, double&&>,
                    ref_binding<bind_info{}, int&>,
                    ref_binding<bind_info{}, char&&>,
                    ref_binding<bind_info{}, int&>,
                    ref_binding<bind_info{}, const char(&)[4]>>>);
  return true;
}

static_assert(check_flatten_bindings());

constexpr auto check_const_and_non_const() {
  double b = 2.3;

  auto bs = non_constant(1, ensure_binding_tuple(b, 'c'));
  constexpr bind_info def_non_const_bi{
      category_projection{}, const_projection::non_constant};
  static_assert(std::is_same_v<
                decltype(bs),
                std::tuple<
                    ref_binding<def_non_const_bi, int&&>,
                    ref_binding<def_non_const_bi, double&>,
                    ref_binding<def_non_const_bi, char&&>>>);

  auto cbs = constant(std::move(bs));
  constexpr bind_info def_const_bi{
      category_projection{}, const_projection::constant};
  static_assert(std::is_same_v<
                decltype(cbs),
                std::tuple<
                    ref_binding<def_const_bi, int&&>,
                    ref_binding<def_const_bi, double&>,
                    ref_binding<def_const_bi, char&&>>>);

  using const_b = std::tuple<ref_binding<def_const_bi, double&>>;
  using non_const_b = std::tuple<ref_binding<def_non_const_bi, double&>>;
  static_assert(std::is_same_v<decltype(constant(b)), const_b>);
  static_assert(std::is_same_v<decltype(constant(non_constant(b))), const_b>);
  static_assert(std::is_same_v<decltype(non_constant(b)), non_const_b>);
  static_assert(
      std::is_same_v<decltype(non_constant(constant(b))), non_const_b>);

  return true;
}

static_assert(check_const_and_non_const());

constexpr auto check_by_ref() {
  double b = 2.3;

  // Of course, the ref to 1 is dangling, but we just need a type
  auto rbs = by_ref(1, ensure_binding_tuple(b, 'c'));
  constexpr bind_info ref_def_bi{category_projection::ref, const_projection{}};
  static_assert(std::is_same_v<
                decltype(rbs),
                std::tuple<
                    ref_binding<ref_def_bi, int&&>,
                    ref_binding<ref_def_bi, double&>,
                    ref_binding<ref_def_bi, char&&>>>);

  constexpr bind_info ref_non_const_bi{
      category_projection::ref, const_projection::non_constant};
  using non_const_refs = std::tuple<
      ref_binding<ref_non_const_bi, int&&>,
      ref_binding<ref_non_const_bi, double&>,
      ref_binding<ref_non_const_bi, char&&>>;

  auto bs1 = non_constant(std::move(rbs));
  static_assert(std::is_same_v<decltype(bs1), non_const_refs>);

  auto bs2 = by_non_const_ref(1, ensure_binding_tuple(b, 'c'));
  static_assert(std::is_same_v<decltype(bs2), non_const_refs>);

  using const_ref = std::tuple<ref_binding<
      bind_info{category_projection::ref, const_projection::constant},
      double&>>;
  static_assert(std::is_same_v<decltype(constant(by_ref(b))), const_ref>);
  static_assert(std::is_same_v<decltype(by_ref(constant(b))), const_ref>);

  using non_const_ref = std::tuple<ref_binding<ref_non_const_bi, double&>>;
  static_assert(std::is_same_v<decltype(by_non_const_ref(b)), non_const_ref>);
  static_assert(
      std::is_same_v<decltype(non_constant(by_ref(b))), non_const_ref>);
  static_assert(
      std::is_same_v<decltype(by_ref(non_constant(b))), non_const_ref>);

  return true;
}

static_assert(check_by_ref());

constexpr auto check_in_place_binding_step_by_step() {
  bool made = false;

  // These vars can't be prvalues since the `Foo` ctor is delayed.
  bool* made_ptr = &made;
  int n = 37;

  // Not a prvalue due to [[clang::lifetimebound]] on `what_to_bind()`.
  auto binding = make_in_place<Foo>(made_ptr, n);
  static_assert(std::is_same_v<
                decltype(binding),
                in_place_binding<bind_info{}, Foo, bool*&, int&>>);
  auto fooMaker = std::move(binding).what_to_bind();
  test(!made);

  Foo foo = std::move(fooMaker);
  test(made);
  test(foo.n_ == n);

  return true;
}

static_assert(check_in_place_binding_step_by_step());

constexpr auto check_in_place_binding_one_line() {
  bool made = false;

  // Here, prvalues are ok since `Foo` is constructed in the same statement.
  Foo foo = make_in_place<Foo>(&made, 37).what_to_bind();
  test(made);
  test(foo.n_ == 37);

  int n = 3;
  Foo f2 = make_in_place<Foo>(nullptr, n).what_to_bind();
  ++n;
  test(3 == f2.n_);
  test(4 == n);

  return true;
}

static_assert(check_in_place_binding_one_line());

// NB: These signatures are NOT meant to be user-visible.
constexpr auto check_in_place_binding_type_sig() {
  static_assert(std::is_same_v<
                decltype(make_in_place<Foo>(nullptr, 7)),
                in_place_binding<bind_info{}, Foo, std::nullptr_t, int>>);

  int n = 7;
  static_assert(std::is_same_v<
                decltype(make_in_place<Foo>(nullptr, n)),
                in_place_binding<bind_info{}, Foo, std::nullptr_t, int&>>);

  // Composes with projection modifiers as expected
  static_assert(
      std::is_same_v<
          decltype(constant(make_in_place<Foo>(nullptr, 7))),
          std::tuple<in_place_binding<
              bind_info{category_projection{}, const_projection::constant},
              Foo,
              std::nullptr_t,
              int>>>);

  return true;
}

static_assert(check_in_place_binding_type_sig());

constexpr auto check_in_place_binding_via_fn() {
  Foo f1 = make_in_place_via([]() { return Foo{nullptr, 17}; }).what_to_bind();
  test(17 == f1.n_);

  auto fn = []() { return Foo{nullptr, 37}; };
  auto b2 = make_in_place_via(fn);
  // This signature isn't really usable by end-users any more, so
  // `make_in_place_via` bindings probably won't show up in headers.
  static_assert(std::is_same_v<
                decltype(b2),
                in_place_binding<
                    bind_info{},
                    Foo,
                    detail::in_place_fn_wrap<decltype(fn)>>>);
  Foo f2 = std::move(b2).what_to_bind();
  test(37 == f2.n_);

  return true;
}

static_assert(check_in_place_binding_via_fn());

constexpr auto check_in_place_binding_modifier_distributive_property() {
  constexpr bind_info def_non_const_bi{
      category_projection{}, const_projection::non_constant};
  constexpr bind_info ref_non_const_bi{
      category_projection::ref, const_projection::non_constant};
  using my_list = std::tuple<
      ref_binding<def_non_const_bi, bool&&>,
      ref_binding<ref_non_const_bi, double&>,
      in_place_binding<def_non_const_bi, int, int>,
      ref_binding<ref_non_const_bi, char&&>>;

  double b = 2;
  static_assert(std::is_same_v<
                my_list,
                decltype(non_constant(
                    true, by_ref(b), make_in_place<int>(3), by_ref('c')))>);
  static_assert(std::is_same_v<
                my_list,
                decltype(non_constant(
                    non_constant(true),
                    by_non_const_ref(b),
                    non_constant(make_in_place<int>(3)),
                    by_non_const_ref('c')))>);

  return true;
}

static_assert(check_in_place_binding_modifier_distributive_property());

template <typename BTup>
using first_policy = binding_policy<std::tuple_element_t<0, BTup>>;

template <typename BTup>
using store = typename first_policy<BTup>::storage_type;

constexpr auto check_in_place_binding_storage_type() {
  int b = 2;

  static_assert(std::is_same_v<store<decltype(constant(b))>, const int>);
  static_assert(std::is_same_v<store<decltype(non_constant(b))>, int>);
  static_assert(std::is_same_v<store<decltype(constant(5))>, const int>);
  static_assert(std::is_same_v<store<decltype(non_constant(5))>, int>);

  static_assert(std::is_same_v<store<decltype(by_ref(b))>, const int&>);
  static_assert(
      std::is_same_v<store<decltype(constant(by_ref(b)))>, const int&>);
  static_assert(std::is_same_v<store<decltype(by_non_const_ref(b))>, int&>);
  static_assert(std::is_same_v<store<decltype(by_ref(5))>, const int&&>);
  static_assert(
      std::is_same_v<store<decltype(constant(by_ref(5)))>, const int&&>);
  static_assert(std::is_same_v<store<decltype(by_non_const_ref(5))>, int&&>);

  static_assert(std::is_same_v<
                binding_policy<decltype(make_in_place<int>(5))>::storage_type,
                int>);
  static_assert(std::is_same_v<
                store<decltype(constant(make_in_place<int>(5)))>,
                const int>);

  return true;
}

static_assert(check_in_place_binding_storage_type());

// A minimal test for `using signature_type = storage_type`...
static_assert(
    std::is_same_v<
        typename first_policy<decltype(constant(by_ref(5)))>::signature_type,
        const int&&>);
