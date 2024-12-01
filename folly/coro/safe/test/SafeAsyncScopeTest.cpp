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
#include <folly/coro/safe/AfterCleanup.h>
#include <folly/coro/safe/AsyncClosure.h>
#include <folly/coro/safe/SafeAsyncScope.h>
#include <folly/fibers/Semaphore.h>
#include <folly/portability/GTest.h>

#ifndef _WIN32 // Explained in SafeTask.h

using namespace folly::bindings;
using namespace std::literals::chrono_literals;

namespace folly::coro {

CO_TEST(SafeAsyncScope, noOp) {
  co_await async_closure(
      [](auto scope) -> ClosureTask<void> {
        static_assert(std::is_same_v<
                      co_cleanup_capture<SafeAsyncScope<CancelViaParent>&>,
                      decltype(scope)>);
        co_return;
      },
      safeAsyncScope<CancelViaParent>());
}

template <typename AScope>
Task<void> check_schedule() {
  int finalBits = co_await async_closure(
      [](auto scope, auto bits1)
          -> ClosureTask<move_after_cleanup_t<std::atomic_int, int>> {
        static_assert(
            std::is_same_v<decltype(scope), co_cleanup_capture<AScope&>>);
        static_assert(
            std::is_same_v<decltype(bits1), capture<std::atomic_int&>>);
        scope->with(co_await co_current_executor)
            .schedule(async_closure(
                [](auto bits2) -> ClosureTask<void> {
                  static_assert(std::is_same_v<
                                decltype(bits2),
                                capture<std::atomic_int&>>);
                  *bits2 += 1;
                  co_return;
                },
                bits1));
        scope->with(co_await co_current_executor)
            .schedule(async_closure(
                [](auto bits2, int toAdd) -> ClosureTask<void> {
                  // Future: Once "restricted ref" support is finished, schedule
                  // another sub-closure on `outerScope` from here.
                  *bits2 += toAdd;
                  co_return;
                },
                bits1,
                2));
        // Verify that we can pass `scope` into a sub-closure
        co_await async_closure(
            [](auto outerScope, auto bits2) -> ClosureTask<void> {
              static_assert(std::is_same_v<
                            decltype(outerScope),
                            co_cleanup_capture<AScope&>>);
              outerScope->with(co_await co_current_executor)
                  .schedule(async_closure(
                      [](auto bits3) -> ClosureTask<void> {
                        *bits3 += 4;
                        co_return;
                      },
                      bits2));
            },
            scope,
            bits1);

        // XXX schedule a bare CoCleanupSafeTask and ValueTask

        // Atomics aren't movable, so convert to `int` after cleanup.
        co_return move_after_cleanup_as<int>(bits1);
      },
      as_capture(make_in_place<AScope>()),
      // Callbacks will mutate this to affect the outer scope.
      as_capture(make_in_place<std::atomic_int>(0)));
  EXPECT_EQ(7, finalBits);
}

CO_TEST(SafeAsyncScope, CancelOnErrorOrRequest__schedule) {
  co_await check_schedule<SafeAsyncScope<CancelOnErrorOrRequest>>();
}
CO_TEST(SafeAsyncScope, CancelOnExitOrRequest__schedule) {
  co_await check_schedule<SafeAsyncScope<CancelOnExitOrRequest>>();
}
CO_TEST(SafeAsyncScope, CancelViaParentOrRequest__schedule) {
  co_await check_schedule<SafeAsyncScope<CancelViaParentOrRequest>>();
}
CO_TEST(SafeAsyncScope, CancelOnRequest__schedule) {
  co_await check_schedule<SafeAsyncScope<CancelOnRequest>>();
}
CO_TEST(SafeAsyncScope, NeverCancel__schedule) {
  co_await check_schedule<SafeAsyncScope<NeverCancel>>();
}
CO_TEST(SafeAsyncScope, CancelViaParent__schedule) {
  co_await check_schedule<SafeAsyncScope<CancelViaParent>>();
}

namespace {
std::atomic_int globalCounter;
}

template <typename CancelPredicate, bool BlockClosure>
Task<void> checkCancelParentToCancelSubTasks() {
  globalCounter = 0;
  // Using `timeoutNoDiscard` to check that the post-cancellation result of
  // `async_closure()` is "success" or `OperationCancelled`, as expected.
  // Regular `timeout` would discard the operation result if too much time
  // has passed, and throw `FutureTimeout` instead of "success".
  auto t = timeoutNoDiscard(
      async_closure(
          [](auto scope) -> ClosureTask<void> {
            scope->with(co_await co_current_executor)
                .schedule([]() -> ValueTask<void> {
                  folly::fibers::Semaphore stuck{0}; // a cancellable baton
                  ++globalCounter;
                  co_await stuck.co_wait();
                  LOG(FATAL) << "Not reached";
                }());
            if constexpr (!BlockClosure) {
              ++globalCounter;
              // The CancelPolicy gets an empty `exception_wrapper`.
              co_return;
            }
            // This branch puts `OperationCancelled` in the `exception_wrapper`.
            folly::fibers::Semaphore stuck{0}; // a cancellable baton
            ++globalCounter;
            co_await stuck.co_wait();
            LOG(FATAL) << "Not reached";
          },
          safeAsyncScope<CancelPredicate>()),
      500ms); // high enough the !BlockClosure path never times out
  if constexpr (BlockClosure) {
    EXPECT_THROW(co_await std::move(t), folly::OperationCancelled);
  } else {
    co_await std::move(t);
  }
  EXPECT_EQ(2, globalCounter.load());
}

CO_TEST(SafeAsyncScope, CancelOnErrorOrRequest__cancelClosureToCancelSubTasks) {
  co_await checkCancelParentToCancelSubTasks<CancelOnErrorOrRequest, true>();
  // !BlockClosure would hang because there is no error.
}
CO_TEST(SafeAsyncScope, CancelOnExitOrRequest__cancelClosureToCancelSubTasks) {
  co_await checkCancelParentToCancelSubTasks<CancelOnExitOrRequest, true>();
  co_await checkCancelParentToCancelSubTasks<CancelOnExitOrRequest, false>();
}
CO_TEST(SafeAsyncScope, CancelViaParent__cancelClosureToCancelSubTasks) {
  co_await checkCancelParentToCancelSubTasks<CancelViaParentOrRequest, true>();
  co_await checkCancelParentToCancelSubTasks<CancelViaParentOrRequest, false>();

  co_await checkCancelParentToCancelSubTasks<CancelViaParent, true>();
  co_await checkCancelParentToCancelSubTasks<CancelViaParent, false>();
}
// `NeverCancel` would block in this test family

// XXX cover requestCancellation, and all policies that support it

/* XXX blocks as expected
CO_TEST(SafeAsyncScope, XXX) {
  co_await checkCancelParentToCancelSubTasks<NeverCancel>();
}

"does not cancel" tests to just cover
 - NeverCancel
 - CancelOnErrorOrRequest

*/

// XXX WIP IGNORE THIS, starting work on the "does not cancel" test
template <typename CancelPredicate>
Task<void> checkClosureSuccessDoesNotCancelSubTasks() {
  globalCounter = 0;
  // XXX WIP need something to kick the `stuck` semaphore after N ms and
  // also need to check that we waited > N ms.
  co_await timeout(
      async_closure(
          [](auto scope) -> ClosureTask<void> {
            scope->with(co_await co_current_executor)
                .schedule([]() -> ValueTask<void> {
                  folly::fibers::Semaphore stuck{0}; // a cancellable baton
                  ++globalCounter;
                  co_await stuck.co_wait();
                  ++globalCounter;
                }());
            // Cleanly exit closure BEFORE the sub-task finishes.
            ++globalCounter;
          },
          safeAsyncScope<CancelPredicate>()),
      100ms); // will be ignored
  EXPECT_EQ(3, globalCounter.load());
}

// XXX test thread-safety of add/schedule: 64 threads race to each schedule
// a callback that does an atomic add of the corresponding bit to a 64-bit #

// A version of `check_schedule` with a recursive `scheduleScopeClosure`.
// This behavior is independent of cancellation policies.
CO_TEST(SafeAsyncScope, scheduleScopeClosure__recurseOnce) {
  int finalBits = co_await async_closure(
      [](auto scope, auto bits1)
          -> ClosureTask<move_after_cleanup_t<std::atomic_int, int>> {
        scope->with(co_await co_current_executor)
            .scheduleScopeClosure(
                [](auto outerScope,
                   auto bits2,
                   auto leet) -> ClosureTask<void> {
                  static_assert( //
                      std::is_same_v<
                          decltype(outerScope),
                          co_cleanup_capture<
                              SafeAsyncScope<CancelViaParent>&>>);
                  // The `after_cleanup_ref_` prefix is an important consequence
                  // of `outerScope` having `shared_cleanup` safety.  It
                  // stops us scheduling sub-closures with refs to `leet`.
                  //
                  // It's not `int&` because we hit the "no outer coro"
                  // optimization -- the args got moved into the inner coro.
                  static_assert(std::is_same_v<
                                decltype(leet),
                                after_cleanup_capture<int>>);
                  *bits2 += 1;
                  outerScope->with(co_await co_current_executor)
                      .scheduleScopeClosure(
                          [](auto /*outerScope*/,
                             auto bits3) -> ClosureTask<void> {
                            *bits3 += 2;
                            co_return;
                          },
                          bits2);
                },
                bits1,
                as_capture(1337));
        co_return move_after_cleanup_as<int>(bits1);
      },
      safeAsyncScope<CancelViaParent>(),
      as_capture(make_in_place<std::atomic_int>(0)));
  EXPECT_EQ(3, finalBits);
}

} // namespace folly::coro

#endif
