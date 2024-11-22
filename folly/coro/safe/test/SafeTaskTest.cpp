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

#include <folly/coro/GtestHelpers.h>
#include <folly/coro/Timeout.h>
#include <folly/coro/safe/SafeTask.h>
#include <folly/fibers/Semaphore.h>
#include <folly/portability/GTest.h>

using namespace folly;
using namespace folly::coro;
using namespace std::literals::chrono_literals;

struct StatelessClass {
  ValueTask<void> validSafeTask() { co_return; }
};

struct StatefulClass {
  int i;
};

TEST(SafeTask, isSafeTaskValid) {
  using folly::coro::detail::IsSafeTaskValid;
  constexpr auto kVal = safe_alias::maybe_value;
  constexpr auto kPost = safe_alias::co_cleanup_safe_ref;
  constexpr auto kPre = safe_alias::body_only_ref;

  // Without an implicit object parameter
  static_assert(IsSafeTaskValid<kVal, int, int>);
  static_assert(!IsSafeTaskValid<kVal, int, int*>);
  static_assert(!IsSafeTaskValid<kVal, int*, int>);
  static_assert(IsSafeTaskValid<kVal, void, int>);
  static_assert(!IsSafeTaskValid<kVal, void, int*>);

  // With an implicit "class" object parameter
  static_assert(IsSafeTaskValid<kVal, int, StatelessClass&, int>);
  static_assert(IsSafeTaskValid<kVal, int, const StatelessClass&, int>);
  static_assert(!IsSafeTaskValid<kVal, int, StatefulClass&, int>);
  static_assert(!IsSafeTaskValid<kVal, int, const StatefulClass&, int>);

  // With an implicit "lambda" object parameter
  auto okFn = [](int x) -> ValueTask<int> { co_return x; };
  static_assert(IsSafeTaskValid<kVal, int, decltype(okFn)&, int>);
  static_assert(IsSafeTaskValid<kVal, int, const decltype(okFn)&, int>);
  // Can declare this with captures because it's not a coro
  auto badFn = [okFn](int x) -> ValueTask<int> { return okFn(x); };
  static_assert(!IsSafeTaskValid<kVal, int, decltype(badFn)&, int>);
  static_assert(!IsSafeTaskValid<kVal, int, const decltype(badFn)&, int>);

  // With a templated implicit object parameter
  auto okTmpl = [](auto x) -> ValueTask<int> { co_return x; };
  static_assert(IsSafeTaskValid<kVal, int, decltype(okTmpl)&, int>);
  static_assert(IsSafeTaskValid<kVal, int, const decltype(okTmpl)&, int>);
  // Can declare this with captures because it's not a coro
  auto badTmpl = [okTmpl](auto x) -> ValueTask<int> { return okTmpl(x); };
  static_assert(!IsSafeTaskValid<kVal, int, decltype(badTmpl)&, int>);
  static_assert(!IsSafeTaskValid<kVal, int, const decltype(badTmpl)&, int>);

  // safe_alias::body_only_ref relaxes constraint on args, but not return val
  static_assert(IsSafeTaskValid<kPre, int, manual_safe_ref_t<kPre, int>>);
  static_assert(IsSafeTaskValid<kPre, int, manual_safe_ref_t<kPost, int>>);
  static_assert(!IsSafeTaskValid<kPre, int, int*>);
  static_assert(!IsSafeTaskValid<kPre, int*, int>);
  static_assert(!IsSafeTaskValid<kPre, manual_safe_ref_t<kPre, int>, int>);
  static_assert(!IsSafeTaskValid<kPre, manual_safe_ref_t<kPost, int>, int>);
  static_assert(IsSafeTaskValid<kPre, void, int>);
  static_assert(!IsSafeTaskValid<kPre, void, int*>);

  // Ditto for safe_alias::co_cleanup_safe_ref
  static_assert(!IsSafeTaskValid<kPost, int, manual_safe_ref_t<kPre, int>>);
  static_assert(IsSafeTaskValid<kPost, int, manual_safe_ref_t<kPost, int>>);
  static_assert(!IsSafeTaskValid<kPost, int, int*>);
  static_assert(!IsSafeTaskValid<kPost, int*, int>);
  static_assert(!IsSafeTaskValid<kPost, manual_safe_ref_t<kPre, int>, int>);
  static_assert(!IsSafeTaskValid<kPost, manual_safe_ref_t<kPost, int>, int>);
  static_assert(IsSafeTaskValid<kPost, void, int>);
  static_assert(!IsSafeTaskValid<kPost, void, int*>);
}

TEST(SafeTask, safe_alias_of_v) {
  static_assert(safe_alias_of_v<ValueTask<int>> == safe_alias::maybe_value);
  static_assert(
      safe_alias_of_v<SafeTask<safe_alias::body_only_ref, int>> ==
      safe_alias::body_only_ref);
}

CO_TEST(SafeTask, trivial) {
  EXPECT_EQ(
      1337, co_await [](int x) -> ValueTask<int> { co_return 1300 + x; }(37));
}

CO_TEST(CoCleanupSafeTask, trivial) {
  int x = 37;
  auto t = [](auto x) -> CoCleanupSafeTask<int> { co_return 1300 + x; };
  EXPECT_EQ(
      1337, co_await t(manual_safe_ref<safe_alias::co_cleanup_safe_ref>(x)));
  EXPECT_EQ(1337, co_await t(manual_safe_ref(x)));
}

CO_TEST(PreCleanupTask, trivial) {
  int x = 37;
  auto t = [](auto x) -> SafeTask<safe_alias::body_only_ref, int> {
    co_return 1300 + x;
  };
  EXPECT_EQ(1337, co_await t(manual_safe_ref<safe_alias::body_only_ref>(x)));
  EXPECT_EQ(
      1337, co_await t(manual_safe_ref<safe_alias::co_cleanup_safe_ref>(x)));
  EXPECT_EQ(1337, co_await t(manual_safe_ref(x)));
}

namespace {
ValueTask<int> intFunc(auto x) {
  co_return *x;
}
} // namespace

CO_TEST(SafeTask, returnsNonVoid) {
  auto x = std::make_unique<int>(17);
  auto lambdaTmpl = [](auto x) -> ValueTask<int> { co_return x; };
  EXPECT_EQ(
      20,
      // Would fail to compile with a raw pointer (i.e. `.get()`)
      co_await intFunc(std::move(x)) + co_await lambdaTmpl(3));
}

namespace {
ValueTask<void> voidFunc(auto x) {
  EXPECT_EQ(17, *x);
  co_return;
}
} // namespace

CO_TEST(SafeTask, returnsVoid) {
  auto lambdaTmpl = [](auto x) -> ValueTask<void> {
    EXPECT_EQ(3, x);
    co_return;
  };
  co_await lambdaTmpl(3);
  auto x = std::make_unique<int>(17);
#if 1
  co_await voidFunc(std::move(x));
#else // Manual test: passing `int*` breaks the build with "Bad SafeTask"
  co_await voidFunc(x.get());
#endif
}

CO_TEST(SafeTask, awaitsTask) {
  EXPECT_EQ(
      1337, co_await []() -> ValueTask<int> {
        co_return 1300 + co_await ([]() -> Task<int> { co_return 37; }());
      }());
}

CO_TEST(SafeTask, cancellation) {
  EXPECT_THROW(
      co_await timeout(
          []() -> ValueTask<void> {
            folly::fibers::Semaphore stuck{0}; // a cancellable baton
            co_await stuck.co_wait();
          }(),
          200ms),
      folly::FutureTimeout);
}

namespace {
struct MyError : std::exception {};
} // namespace

CO_TEST(SafeTask, throws) {
  EXPECT_THROW(
      co_await []() -> ValueTask<void> { co_yield co_error(MyError{}); }(),
      MyError);
}

CO_TEST(SafeTask, co_awaitTry) {
  auto res = co_await co_awaitTry(
      []() -> ValueTask<void> { co_yield co_error(MyError{}); }());
  EXPECT_TRUE(res.hasException<MyError>());
}

namespace folly::coro::detail {

struct SafeTaskTest : testing::Test {
  template <safe_alias NewSafety>
  auto withNewSafety(auto t) {
    return std::move(t).template withNewSafety<NewSafety>();
  }
};

// DO NOT COPY THIS!  `withNewSafety` is a hacks meant EXCLUSIVELY for the
// `async_closure()` implementation.
CO_TEST_F(SafeTaskTest, withNewSafety) {
  int x = 7;
  auto t = withNewSafety<safe_alias::maybe_value>(
      [](auto x) -> SafeTask<safe_alias::shared_cleanup, int> {
        co_return 30 + x;
      }(manual_safe_ref<safe_alias::shared_cleanup>(x)));
  static_assert(std::is_same_v<decltype(t), ValueTask<int>>);
  EXPECT_EQ(37, co_await std::move(t));
}

CO_TEST_F(SafeTaskTest, ClosureTask) {
  int x = 37;
  auto t = [](auto x) -> ClosureTask<int> { co_return 1300 + x; };
  // These must be unwrapped to be awaited. The "new safety" is incidental.
  EXPECT_EQ(
      1337,
      co_await withNewSafety<safe_alias::maybe_value>(
          t(manual_safe_ref<safe_alias::shared_cleanup>(x))));
  EXPECT_EQ(
      1337,
      co_await withNewSafety<safe_alias::maybe_value>(t(manual_safe_ref(x))));
}

struct HasMemberTask {
  MemberTask<int> task(auto x) { co_return 1300 + x; }
};

static_assert(!std::is_move_constructible_v<MemberTask<int>>);
static_assert(!std::is_move_assignable_v<MemberTask<int>>);

CO_TEST_F(SafeTaskTest, MemberTask) {
  HasMemberTask mt;
  int x = 37;
  EXPECT_EQ(1337, co_await mt.task(x));
  EXPECT_EQ(
      1337, co_await mt.task(manual_safe_ref<safe_alias::shared_cleanup>(x)));
  EXPECT_EQ(1337, co_await mt.task(manual_safe_ref(x)));
}

CO_TEST(SafeTask, scheduleOnSafe) {
  auto te = ([]() -> CoCleanupSafeTask<int> { co_return 37; }())
                .scheduleOnSafe(co_await co_current_executor);
  static_assert(std::is_same_v<
                decltype(te),
                SafeTaskWithExecutor<safe_alias::co_cleanup_safe_ref, int>>);
  EXPECT_EQ(37, co_await std::move(te).unwrap());
}

} // namespace folly::coro::detail
