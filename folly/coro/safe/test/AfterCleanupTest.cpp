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
#include <folly/coro/safe/AfterCleanup.h>
#include <folly/coro/safe/AsyncClosure.h>

#ifndef _WIN32 // Explained in SafeTask.h

using namespace folly;
using namespace folly::coro;
using namespace folly::bindings;

struct Cleanup {
  std::optional<capture<int&>> total_;
  Task<void> co_cleanup(async_closure_private_t) {
    // Detects order of operations: +* isn't the same as *+
    **total_ *= 10;
    co_return;
  }
};

CO_TEST(AfterCleanup, move) {
  int res = co_await async_closure(
      [](auto cleanup, auto total) -> ClosureTask<move_after_cleanup_t<int>> {
        cleanup->total_ = total;
        *total += 2; // 9
        co_return move_after_cleanup(total); // 90
      },
      as_capture(Cleanup{}),
      as_capture(7));
  EXPECT_EQ(90, res);
}

CO_TEST(AfterCleanup, moveAs) {
  auto res = co_await async_closure(
      [](auto cleanup,
         auto total) -> ClosureTask<move_after_cleanup_t<int, char>> {
        cleanup->total_ = total;
        *total += 2; // 9
        co_return move_after_cleanup_as<char>(total); // 90
      },
      as_capture(Cleanup{}),
      as_capture(7));
  static_assert(std::is_same_v<char, decltype(res)>);
  EXPECT_EQ(static_cast<char>(90), res);
}

CO_TEST(AfterCleanup, noOuterCoroMove) {
  int res = co_await async_closure(
      [](capture<int> total) -> ClosureTask<int> {
        *total += 2; // 9
        co_return move_after_cleanup(total); // still 9
      },
      as_capture(7));
  EXPECT_EQ(9, res);
}

CO_TEST(AfterCleanup, noOuterCoroMoveAs) {
  auto res = co_await async_closure(
      [](capture_heap<std::atomic_int> n) -> ClosureTask<char> {
        co_return move_after_cleanup_as<char>(n);
      },
      as_capture(make_in_place<std::atomic_int>('@')));
  static_assert(std::is_same_v<char, decltype(res)>);
  EXPECT_EQ('@', res);
}

// XXX test capture_unique

#if 0 // XXX
CO_TEST(AfterCleanupTest, co_scheduleClosure_MoveAfterJoin) {
  EXPECT_EQ(
      "hello stranger",
      co_await asyncClosure(
          [](auto& scope) -> ClosureTask<MoveAfterJoin<std::string>> {
            scope["s"_id] += " strange";
            co_await scope.co_scheduleClosure(
                [](auto& scope) -> ClosureTask<void> {
                  scope["s"_id] += "r";
                  co_return;
                });
            // The background task may run after `co_return`; `moveAfterJoin()`
            // moves this field AFTER joining the internal `AsyncScope`.
            co_return scope.moveAfterJoin("s"_id);
          },
          "s"_id = asNonConst(std::string{"hello"})));
}

CO_TEST(AfterCleanupTest, co_scheduleClosure_NamedMoveAfterJoin) {
  auto res = co_await asyncClosure(
      [](auto& scope) -> ClosureTask<NamedMoveAfterJoin<
                          "a"_id.type<std::unique_ptr<int>>,
                          "b1"_id.type<std::unique_ptr<int>>>> {
        co_await scope.co_scheduleClosure(
            [](auto& scope) -> ClosureTask<void> {
              *scope["a"_id] += 700;
              co_return;
            });
        co_await scope.co_scheduleClosure(
            [](auto& scope) -> ClosureTask<void> {
              *scope["b"_id] += 400;
              co_return;
            });
        *scope["a"_id] += 7;
        *scope["b"_id] += 3;
        // The background tasks may run after `co_return`; `moveAfterJoin()`
        // moves these field AFTER joining the internal `AsyncScope`.
        co_return scope.namedMoveAfterJoin("a"_id, "b1"_id = "b"_id);
      },
      // `unique_ptr` ensures the implementation doesn't copy by accident
      "a"_id = asNonConst(std::make_unique<int>(0)),
      "b"_id = asNonConst(std::make_unique<int>(0)));
  EXPECT_EQ(707, *res["a"_id]);
  EXPECT_EQ(404, ++(*res["b1"_id])); // the `NamedValues` are NOT const
}
#endif

#endif
