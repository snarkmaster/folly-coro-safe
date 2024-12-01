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

#include <folly/coro/safe/Captures.h>
#include <folly/lang/Bindings.h>
#include <folly/portability/GTest.h>

#ifdef FOLLY_MOVABLE_AND_DEEP_CONST_LREF_COPYABLE
static_assert(false, "Leaked FOLLY_MOVABLE_AND_DEEP_CONST_LREF_COPYABLE macro");
#endif

#ifndef _WIN32 // Explained in SafeTask.h

namespace folly::coro::detail {

struct SimpleCleanup {
  void co_cleanup(async_closure_private_t) {}
  int x() { return 7; }
  void canMutate() {}
};

decltype(auto) get_address(const auto& v) {
  return &v;
}

template <typename TA, typename TB>
void check_capture_same_address(TA&& a, TB&& b) {
  EXPECT_EQ(
      get_address(*std::forward<TA>(a)), get_address(*std::forward<TB>(b)));
}

struct CapturesTest : testing::Test {
  // Required to access friend-only ctor of `capture`s.
  static constexpr capture_private_t arg_priv{};

  template <typename ArgT>
  auto make(auto&& arg) {
    return ArgT{
        arg_priv,
        folly::bindings::detail::ensure_binding(
            std::forward<decltype(arg)>(arg))};
  }

  template <typename Arg>
  auto shared_cleanup_ref(Arg&& arg) {
    return std::forward<Arg>(arg).template to_capture_ref<true>(arg_priv);
  }
  template <typename Arg>
  auto independent_cleanup_ref(Arg&& arg) {
    return std::forward<Arg>(arg).template to_capture_ref<false>(arg_priv);
  }

  template <template <typename> class RefArg = capture, typename T = int>
  void check_ref_from_mutable(int expected, auto a, auto make_ref) {
    {
      auto ar = make_ref(a);
      static_assert(std::is_same_v<decltype(ar), RefArg<T&>>);
      EXPECT_EQ(expected, *ar);
      *a += 10;
      expected += 10;

      auto ar2 = make_ref(ar);
      static_assert(std::is_same_v<decltype(ar2), RefArg<T&>>);
      EXPECT_EQ(expected, *ar2);

      // Future: How to concisely test that just `T&` won't compile?
      auto ar3 = make_ref(std::as_const(ar2));
      static_assert(std::is_same_v<decltype(ar3), RefArg<const T&>>);
      EXPECT_EQ(expected, *ar3);

      // Future: How to test here (and below) that `std::move` is required?
      auto ar4 = make_ref(std::move(ar2));
      static_assert(std::is_same_v<decltype(ar4), RefArg<T&&>>);
      EXPECT_EQ(expected, *std::move(ar4));
    }
    {
      auto ar = make_ref(std::move(a));
      static_assert(std::is_same_v<decltype(ar), RefArg<T&&>>);
      EXPECT_EQ(expected, *std::move(ar));
      *a -= 10;
      expected -= 10;

      auto ar2 = make_ref(std::move(ar));
      static_assert(std::is_same_v<decltype(ar2), RefArg<T&&>>);
      EXPECT_EQ(expected, *std::move(ar2));
    }
  }

  template <template <typename> class RefArg = capture, typename T = int>
  void check_ref_from_const(int expected, auto a, auto make_ref) {
    {
      auto ar = make_ref(a);
      static_assert(std::is_same_v<decltype(ar), RefArg<const T&>>);
      check_capture_same_address(a, ar);

      auto ar2 = make_ref(ar);
      static_assert(std::is_same_v<decltype(ar2), RefArg<const T&>>);
      check_capture_same_address(a, ar2);

      auto ar3 = make_ref(std::move(ar2));
      static_assert(std::is_same_v<decltype(ar3), RefArg<const T&&>>);
      EXPECT_EQ(expected, *std::move(ar3));
    }
    {
      auto ar = make_ref(std::move(a));
      static_assert(std::is_same_v<decltype(ar), RefArg<const T&&>>);

      auto ar2 = make_ref(std::move(ar));
      static_assert(std::is_same_v<decltype(ar2), RefArg<const T&&>>);
      EXPECT_EQ(expected, *std::move(ar2));
    }
  }

  template <
      typename T = SimpleCleanup,
      template <typename> class RefArg = co_cleanup_capture>
  void check_ref_from_cleanup(auto a, auto make_ref) {
    auto ar = make_ref(a);
    static_assert(std::is_same_v<decltype(ar), RefArg<T&>>);
    check_capture_same_address(a, ar);
    if constexpr (!std::is_const_v<T>) {
      a->canMutate();
    }

    auto ar2 = make_ref(ar);
    static_assert(std::is_same_v<decltype(ar2), RefArg<T&>>);
    check_capture_same_address(a, ar2);

    // Future: How to concisely test that just `T&` won't compile?
    auto ar3 = make_ref(std::as_const(ar2));
    static_assert(std::is_same_v<decltype(ar3), RefArg<const T&>>);
    check_capture_same_address(a, ar3);
  }

  template <
      template <typename>
      class RefFromAfterCleanup,
      template <typename>
      class IndirectRefFromAfterCleanup>
  void check_to_capture_ref(auto make_ref_fn) {
    SimpleCleanup sc;

    check_ref_from_mutable(5, make<capture<int>>(5), make_ref_fn);
    check_ref_from_mutable(5, make<capture_heap<int>>(5), make_ref_fn);
    check_ref_from_mutable<capture_indirect, std::unique_ptr<int>>(
        5,
        make<capture_indirect<std::unique_ptr<int>>>(std::make_unique<int>(5)),
        make_ref_fn);
    check_ref_from_cleanup(
        make<co_cleanup_capture<SimpleCleanup>>(sc), make_ref_fn);

    // Same, but with a `const` type -- no mutability assertions.
    check_ref_from_const(7, make<capture<const int>>(7), make_ref_fn);
    check_ref_from_const(7, make<capture_heap<const int>>(7), make_ref_fn);
    check_ref_from_const<capture_indirect, std::unique_ptr<int>>(
        7,
        make<capture_indirect<const std::unique_ptr<int>>>(
            std::make_unique<int>(7)),
        make_ref_fn);
    check_ref_from_cleanup<const SimpleCleanup>(
        make<co_cleanup_capture<const SimpleCleanup>>(sc), make_ref_fn);

    // Repeat the above blocks, checking whether we shed `after_cleanup_ref_`
    // from the ref.  There is no `after_cleanup_ref_co_cleanup_capture`, of
    // course.

    check_ref_from_mutable<RefFromAfterCleanup>(
        5, make<after_cleanup_capture<int>>(5), make_ref_fn);
    check_ref_from_mutable<RefFromAfterCleanup>(
        5, make<after_cleanup_capture_heap<int>>(5), make_ref_fn);
    check_ref_from_mutable<IndirectRefFromAfterCleanup, std::unique_ptr<int>>(
        5,
        make<after_cleanup_capture_indirect<std::unique_ptr<int>>>(
            std::make_unique<int>(5)),
        make_ref_fn);

    check_ref_from_const<RefFromAfterCleanup>(
        7, make<after_cleanup_capture<const int>>(7), make_ref_fn);
    check_ref_from_const<RefFromAfterCleanup>(
        7, make<after_cleanup_capture_heap<const int>>(7), make_ref_fn);
    check_ref_from_const<IndirectRefFromAfterCleanup, std::unique_ptr<int>>(
        7,
        make<after_cleanup_capture_indirect<const std::unique_ptr<int>>>(
            std::make_unique<int>(7)),
        make_ref_fn);
  }
};

TEST_F(CapturesTest, indirect__get_underlying) {
  auto ci = make<capture_indirect<std::unique_ptr<short>>>(
      std::make_unique<short>(37));
  auto x = std::move(ci).get_underlying();
  static_assert(std::is_same_v<decltype(x), std::unique_ptr<short>>);
  EXPECT_EQ(37, *x);
}

TEST_F(CapturesTest, to_capture_ref__shared_cleanup) {
  // We set `SharedCleanup == true`, so an input arg type with a
  // `after_cleanup_ref_` prefix will retain that prefix on the ref.
  check_to_capture_ref<after_cleanup_capture, after_cleanup_capture_indirect>(
      [&](auto&& arg) {
        return shared_cleanup_ref(std::forward<decltype(arg)>(arg));
      });
}

TEST_F(CapturesTest, to_capture_ref__independent_cleanup) {
  // "Upgrade" behavior: We set `SharedCleanup == false`, so all arg types
  // emit `capture` refs, even when the input was `after_cleanup_ref_`.
  check_to_capture_ref<capture, capture_indirect>([&](auto&& arg) {
    return independent_cleanup_ref(std::forward<decltype(arg)>(arg));
  });
}

TEST_F(CapturesTest, capture__implicit_ref_conversion) {
  // Implicit conversion to capture refs parallels
  // `to_capture_ref__shared_cleanup` -- we must not upgrade
  // `after_cleanup_ref_async_arc*` to non-`after_cleanup_ref`, since we're not
  // creating an independent scope for the new capture ref.
  check_to_capture_ref<after_cleanup_capture, after_cleanup_capture_indirect>(
      [&](auto&& arg) {
        return capture_implicit_ref_t<decltype(arg)>{
            std::forward<decltype(arg)>(arg)};
      });

  // Sample no-abstraction checks -- `check_to_capture_ref` has more coverage
  {
    auto a = make<capture<int>>(5);
    capture<const int&> aclr = std::as_const(a);
    check_capture_same_address(a, aclr);
    capture<int&&> arr = std::move(a);
    check_capture_same_address(a, std::move(arr));
    // Explicitly converting an rref into an lref is NOT covered by
    // `check_to_capture_ref` because that logic is not part of
    // async_closure binding conversions.
    auto alr = capture<int&>{std::move(arr)};
    check_capture_same_address(a, std::move(alr));
  }
  {
    auto a = make<capture<const int>>(5);
    capture<const int&> aclr = std::as_const(a);
    check_capture_same_address(a, std::move(aclr));
    capture<const int&&> acrr = std::move(aclr);
    check_capture_same_address(a, std::move(acrr));
  }
  {
    SimpleCleanup sc;
    auto a = make<co_cleanup_capture<SimpleCleanup>>(sc);
    co_cleanup_capture<SimpleCleanup&> ar = a;
    check_capture_same_address(a, std::move(ar));
  }

  // Check `capture_implicit_ref_t` which is intended for finding the
  // reference type, to which an capture instance can be implicitly
  // converted.

  // Simple mutable refs
  static_assert(
      std::is_same_v<capture_implicit_ref_t<capture<int>&>, capture<int&>>);
  static_assert(
      std::is_same_v<capture_implicit_ref_t<capture<int>>, capture<int&&>>);
  static_assert(
      std::is_same_v<capture_implicit_ref_t<capture<int>&&>, capture<int&&>>);

  // Same, with a `const` wrapped type, ensuring we don't strip `const`
  static_assert(std::is_same_v<
                capture_implicit_ref_t<capture<const int>&>,
                capture<const int&>>);
  static_assert(std::is_same_v<
                capture_implicit_ref_t<capture<const int>>,
                capture<const int&&>>);
  static_assert(std::is_same_v<
                capture_implicit_ref_t<capture<const int>&&>,
                capture<const int&&>>);

  // `const` from the wrapper moves to the wrapped type
  static_assert(std::is_same_v<
                capture_implicit_ref_t<const capture<int>&>,
                capture<const int&>>);
  static_assert(std::is_same_v<
                capture_implicit_ref_t<const capture<int>>,
                capture<const int&&>>);

  // The outer reference type clobbers the inner one.  We DON'T want the
  // `lref&&`-remains-an-lref reference collapsing rule here, since it seems
  // desirable for this to work:
  //   capture<T&&> myRref = std::move(myLref);
  static_assert(
      std::is_same_v<capture_implicit_ref_t<capture<int&&>&>, capture<int&>>);
  static_assert(
      std::is_same_v<capture_implicit_ref_t<capture<int&>&>, capture<int&>>);
  static_assert(
      std::is_same_v<capture_implicit_ref_t<capture<int&>&&>, capture<int&&>>);
  static_assert(
      std::is_same_v<capture_implicit_ref_t<capture<int&&>&&>, capture<int&&>>);
}

// XXX explain in-placy-binding
TEST_F(CapturesTest, XXX) {
  auto binding = folly::bindings::make_in_place<std::string>("hi");
  std::tuple<capture<std::string>> tup{
      folly::bindings::make_in_place_via([&]() {
        return capture<std::string>{arg_priv, std::move(binding)};
      }).what_to_bind()};
}

TEST_F(CapturesTest, noCustomDereference) {
  static_assert(has_async_closure_co_cleanup<SimpleCleanup>);

  auto sc = make<co_cleanup_capture<SimpleCleanup>>(SimpleCleanup{});
  EXPECT_EQ(7, sc->x());

  auto binding = folly::bindings::detail::ensure_binding(*sc);
  auto rsc = restricted_co_cleanup_capture<SimpleCleanup&>{
      arg_priv, std::move(binding)};
#if 0 // Manual test: won't compile due to missing
      // `capture_restricted_ptr_proxy`
  static_assert(!requires { rsc->x(); });
#endif
}

struct CustomDerefCleanupRef : NonCopyableNonMovable {
  explicit CustomDerefCleanupRef(int y) : y_(y) {}
  auto operator->() { return static_cast<CustomDerefCleanupRef*>(this); }
  int y_;
};

struct CustomDerefCleanup {
  void co_cleanup(async_closure_private_t) {}
  friend auto capture_lref_proxy(capture_private_t, CustomDerefCleanup&) {
    return CustomDerefCleanupRef{101};
  }
  friend auto capture_ptr_proxy(capture_private_t, CustomDerefCleanup&) {
    return CustomDerefCleanupRef{202};
  }
  friend auto capture_restricted_lref_proxy(
      capture_private_t, CustomDerefCleanup&) {
    return CustomDerefCleanupRef{303};
  }
  friend auto capture_restricted_ptr_proxy(
      capture_private_t, CustomDerefCleanup&) {
    return CustomDerefCleanupRef{404};
  }
};

TEST_F(CapturesTest, customDereference) {
  static_assert(has_async_closure_co_cleanup<CustomDerefCleanup>);

  auto c = make<co_cleanup_capture<CustomDerefCleanup>>(CustomDerefCleanup{});
  EXPECT_EQ(101, (*c).y_);
  EXPECT_EQ(202, c->y_);

  EXPECT_EQ(202, shared_cleanup_ref(c)->y_);

  CustomDerefCleanup c2;
  auto binding = folly::bindings::detail::ensure_binding(c2);
  auto rc = restricted_co_cleanup_capture<CustomDerefCleanup&>{
      arg_priv, std::move(binding)};

  EXPECT_EQ(303, (*rc).y_);
  EXPECT_EQ(404, rc->y_);
}

// XXX verify that BindingT makes sense const- & ref-wise
// XXX verify ref->ref and val->ref conversions

// XXX test inplace, and that it fails with refs
// XXX construct each kind of arg

// XXX For each:
//  - lightly test copy/movability
//  - safe_alias_of_v

// XXX especially validate that "move-once" things are unsafe
// => maybe add a base class like `must_stay_in_original_closure`,
// and test against it

} // namespace folly::coro::detail

#endif
