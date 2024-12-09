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

#include <folly/coro/BlockingWait.h>
#include <folly/coro/GtestHelpers.h>
#include <folly/coro/safe/NowTask.h>

#ifndef _WIN32 // Explained in NowTask.h

namespace folly::coro {

NowTask<int> leetTask(int x) {
  co_return 1300 + x;
}

CO_TEST(NowTaskTest, trivial) {
  EXPECT_EQ(1337, co_await leetTask(37));
}

// Both of these are antipatterns with `Task` because if you awaited either
// of these coros outside of the statement that created them, it would have
// dangling refs.
CO_TEST(NowTaskTest, passByRef) {
  auto res = co_await [](int&& x) -> NowTask<int> { co_return 1300 + x; }(37);
  EXPECT_EQ(1337, res);
}
CO_TEST(NowTaskTest, lambdaWithCaptures) {
  int a = 1300, b = 37;
  auto res = co_await [&a, b]() -> NowTask<int> { co_return a + b; }();
  EXPECT_EQ(1337, res);
}

auto invokeAndBlockingWait(NowTask<int> t) {
  return folly::coro::blockingWait(std::move(t));
}

template <typename T>
concept InvocableWithMovedArg =
    requires(T t) { invokeAndBlockingWait(std::move(t)); };

CO_TEST(NowTaskTest, cannotDelayAwait) {
  // Since C++17, calling this with a prvalue doesn't involve a move
  // constructor, so there's no violation of the `NowTask` guarantee.
  static_assert(requires { invokeAndBlockingWait(leetTask(37)); });
  EXPECT_EQ(1337, invokeAndBlockingWait(leetTask(37)));

  auto t = leetTask(37);
  // We can't actually make these calls, they'd fail to compile. But, the goal
  // is to show that we can't `co_await std::move(t)` since it's nonmovable.
  static_assert( //
      !std::is_invocable_v<
          decltype(&invokeAndBlockingWait),
          std::remove_reference_t<decltype(t)>>);
  static_assert(!InvocableWithMovedArg<decltype(t)>);
  co_return;
}

CO_TEST(NowTaskTest, toNowTask) {
  static_assert(std::is_same_v<NowTask<int>, decltype(toNowTask(leetTask(5)))>);
  auto t = []() -> Task<int> { co_return 5; }();
  static_assert(
      std::is_same_v<NowTask<int>, decltype(toNowTask(std::move(t)))>);
  EXPECT_EQ(5, co_await toNowTask(std::move(t)));
}

} // namespace folly::coro

#endif
