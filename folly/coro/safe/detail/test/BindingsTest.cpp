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

#include <folly/coro/Task.h>
#include <folly/coro/safe/detail/Bindings.h>
#include <folly/portability/GTest.h>

#ifndef _WIN32 // Explained in SafeTask.h

using folly::bindings::by_ref;
using folly::bindings::constant;
using folly::bindings::make_in_place;
using folly::bindings::detail::bind_info;
using folly::bindings::detail::category_projection;
using folly::bindings::detail::const_projection;
using folly::bindings::detail::ref_binding;

constexpr bind_info bind_info_def_const{
    category_projection{}, const_projection::constant};
constexpr bind_info bind_info_ref_def{
    category_projection::ref, const_projection{}};

namespace folly::coro::detail {

struct HasCleanup {
  Task<void> co_cleanup(async_closure_private_t) { co_return; }
};

constexpr capture_private_t coro_safe_detail_bindings_test_private() {
  return {};
}
static constexpr auto priv = coro_safe_detail_bindings_test_private();

// This is here so that test "runs" show up in CI history
TEST(BindingsTest, all_tests_run_at_build_time) {}

constexpr bool check_empty() {
  static_assert(std::is_same_v<
                decltype(transform_capture_bindings</*force outer*/ false>()),
                std::pair<vtag_t<>, std::tuple<>>>);
  return true;
}

static_assert(check_empty());

template <
    typename ExpectedT,
    safe_alias ExpectedSafety = safe_alias::maybe_value>
constexpr void check_one_no_shared_cleanup(auto&& arg) {
  static_assert(std::is_same_v<
                decltype(transform_capture_bindings</*force outer*/ false>(
                    std::forward<decltype(arg)>(arg))),
                std::pair<vtag_t<ExpectedSafety>, std::tuple<ExpectedT>>>);
}

constexpr auto bind_one(auto&& arg) {
  return std::move(std::get<0>(
      folly::bindings::ensure_binding_tuple(std::forward<decltype(arg)>(arg))));
}

template <
    typename ExpectedT,
    safe_alias ExpectedSafety = safe_alias::maybe_value>
constexpr void check_one_shared_cleanup(auto&& arg) {
  static_assert(std::is_same_v<
                decltype(transform_capture_bindings</*force outer*/ false>(
                    std::forward<decltype(arg)>(arg),
                    std::declval<co_cleanup_capture<HasCleanup&>&>())),
                std::pair<
                    vtag_t<ExpectedSafety, safe_alias::shared_cleanup>,
                    std::tuple<ExpectedT, co_cleanup_capture<HasCleanup&>>>>);
}

template <
    typename ExpectedT,
    safe_alias ExpectedSafety = safe_alias::maybe_value>
constexpr void check_one(auto&& arg) {
  check_one_no_shared_cleanup<ExpectedT, ExpectedSafety>(
      std::forward<decltype(arg)>(arg));
  check_one_shared_cleanup<ExpectedT, ExpectedSafety>(
      std::forward<decltype(arg)>(arg));
}

struct MoveMe : folly::MoveOnly {
  int x;
};

// Somewhat redundant with `lang/Bindings.h` tests, but we should show that
// these work "as expected".  No `make_in_place` coverage here since we
// don't allow those on the "regular" path.
constexpr bool check_regular_args() {
  check_one<ref_binding<bind_info{}, int&&>>(5);
  check_one<ref_binding<bind_info_def_const, int&&>>(constant(5));
  check_one<ref_binding<bind_info_ref_def, int&&>, safe_alias::unsafe>(
      by_ref(5));

  int x = 7;
  check_one<ref_binding<bind_info{}, int&>>(x);
  check_one<ref_binding<bind_info_def_const, int&>>(constant(x));
  check_one<ref_binding<bind_info_ref_def, int&>, safe_alias::unsafe>(
      by_ref(x));

  MoveMe moo{.x = 7};
  check_one<ref_binding<bind_info{}, MoveMe&&>>(std::move(moo));
  check_one<ref_binding<bind_info_def_const, MoveMe&&>>(
      constant(std::move(moo)));
  check_one<ref_binding<bind_info_ref_def, MoveMe&&>, safe_alias::unsafe>(
      by_ref(std::move(moo)));

  return true;
}

static_assert(check_regular_args());

// `check_capture_*_to_ref()` should closely parallel the corresponding
// `Captures.h` tests, but we still need to check the plumbing end-to-end.

constexpr bool check_capture_val_to_ref() {
  { // makeRef1: implicitly convert `capture<Val>` to `capture<Ref>`
    capture<int> av{priv, bind_one(5)};
    check_one<capture<int&>, safe_alias::co_cleanup_safe_ref>(av);
    check_one<capture<int&&>, safe_alias::co_cleanup_safe_ref>(std::move(av));
  }

  { // makeRef1: auto-upgrade `after_cleanup_capture<Val>` to `capture<Ref>`
    after_cleanup_capture<int> av{priv, bind_one(5)};
    check_one_no_shared_cleanup<capture<int&>, safe_alias::after_cleanup_ref>(
        av);
    check_one_no_shared_cleanup<capture<int&&>, safe_alias::after_cleanup_ref>(
        std::move(av));
  }

  { // makeRef1: no automatic upgrade due to `shared_cleanup` arg
    after_cleanup_capture<int> av{priv, bind_one(5)};
    check_one_shared_cleanup<
        after_cleanup_capture<int&>,
        safe_alias::after_cleanup_ref>(av);
    check_one_shared_cleanup<
        after_cleanup_capture<int&&>,
        safe_alias::after_cleanup_ref>(std::move(av));
  }

  { // makeRef2: `by_ref(capture<Val>)` becomes `capture<Ref>`
    capture<int> av{priv, bind_one(5)};
    check_one<capture<int&>, safe_alias::co_cleanup_safe_ref>(by_ref(av));
    check_one<capture<int&&>, safe_alias::co_cleanup_safe_ref>(
        by_ref(std::move(av)));
  }

  // Don't repeat the other `makeRef1` tests for `makeRef2` since the same
  // code path implements both.

  return true;
}

static_assert(check_capture_val_to_ref());

constexpr bool check_capture_lref_to_lref() {
  int x = 5;
  // forwardRef: Copy `capture<V&>` bound as lref
  {
    auto lbind_x = bind_one(x);
    capture<int&> ar{priv, std::move(lbind_x)};
    check_one<capture<int&>, safe_alias::co_cleanup_safe_ref>(ar);
  }

  // forwardRef: Upgrade `after_cleanup_capture<V&>` bound as lref
  {
    auto lbind_x = bind_one(x);
    after_cleanup_capture<int&> ar{priv, std::move(lbind_x)};
    check_one_no_shared_cleanup<capture<int&>, safe_alias::after_cleanup_ref>(
        ar);
  }

  // forwardRef: Do NOT upgrade `after_cleanup_capture<V&>`
  {
    auto lbind_x = bind_one(x);
    after_cleanup_capture<int&> ar{priv, std::move(lbind_x)};
    check_one_shared_cleanup<
        after_cleanup_capture<int&>,
        safe_alias::after_cleanup_ref>(ar);
  }

  // forwardRef: Copy `co_cleanup_capture<V&>` bound as lref
  {
    HasCleanup cleanup;
    auto lbind_cleanup = bind_one(cleanup);
    co_cleanup_capture<HasCleanup&> ar{priv, std::move(lbind_cleanup)};
    check_one<co_cleanup_capture<HasCleanup&>, safe_alias::shared_cleanup>(ar);
  }

  // Manual test: binding `capture<V&&>` as an lvalue won't compile
  {
    auto rbind_x = bind_one(std::move(x));
    capture<int&&> ar{priv, std::move(rbind_x)};
#if 0
    check_one<capture<int&&>, safe_alias::co_cleanup_safe_ref>(ar);
#endif
  }

  return true;
}

static_assert(check_capture_lref_to_lref());

constexpr bool check_capture_lref_to_rref() {
  int x = 5;
  // forwardRef: `capture<V&>` bound as rref -> `capture<V&&>
  {
    auto lbind_x = bind_one(x);
    check_one<capture<int&&>, safe_alias::co_cleanup_safe_ref>(
        capture<int&>{priv, std::move(lbind_x)});
  }

  // forwardRef: Upgrade `after_cleanup_capture<V&>` while moving
  {
    auto lbind_x = bind_one(x);
    check_one_no_shared_cleanup<capture<int&&>, safe_alias::after_cleanup_ref>(
        after_cleanup_capture<int&>{priv, std::move(lbind_x)});
  }

  // forwardRef: Do NOT upgrade `after_cleanup_capture<V&>` while moving
  {
    auto lbind_x = bind_one(x);
    check_one_shared_cleanup<
        after_cleanup_capture<int&&>,
        safe_alias::after_cleanup_ref>(
        after_cleanup_capture<int&>{priv, std::move(lbind_x)});
  }

  // Manual test: Cannot move cleanup arg refs
  {
    HasCleanup cleanup;
    auto lbind_cleanup = bind_one(cleanup);
    co_cleanup_capture<HasCleanup&> ar{priv, std::move(lbind_cleanup)};
#if 0
    transform_capture_bindings</*force outer*/ false>(std::move(ar));
#endif
  }

  return true;
}

static_assert(check_capture_lref_to_rref());

constexpr bool check_capture_rref_to_rref() {
  int x = 5;
  // forwardRef: `capture<V&>` bound as rref -> `capture<V&&>
  {
    auto rbind_x = bind_one(std::move(x));
    check_one<capture<int&&>, safe_alias::co_cleanup_safe_ref>(
        capture<int&&>{priv, std::move(rbind_x)});
  }

  // forwardRef: Upgrade `after_cleanup_capture<V&>` while moving
  {
    auto rbind_x = bind_one(std::move(x));
    check_one_no_shared_cleanup<capture<int&&>, safe_alias::after_cleanup_ref>(
        after_cleanup_capture<int&&>{priv, std::move(rbind_x)});
  }

  // forwardRef: Do NOT upgrade `after_cleanup_capture<V&>` while moving
  {
    auto rbind_x = bind_one(std::move(x));
    check_one_shared_cleanup<
        after_cleanup_capture<int&&>,
        safe_alias::after_cleanup_ref>(
        after_cleanup_capture<int&&>{priv, std::move(rbind_x)});
  }

  return true;
}

static_assert(check_capture_rref_to_rref());

// XXX check_stored* -- including "force outer coro" set to true & false

// XXX check "`guess_binding_cfg` matched reality" vs "not" branches

} // namespace folly::coro::detail

#endif
