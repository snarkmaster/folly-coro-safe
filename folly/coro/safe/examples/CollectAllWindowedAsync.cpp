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

#include <folly/coro/AsyncGenerator.h>
#include <folly/coro/GtestHelpers.h>
#include <folly/coro/Timeout.h>
#include <folly/coro/safe/AsyncClosure.h>
#include <folly/coro/safe/SafeAsyncScope.h>
#include <folly/coro/safe/SafeTask.h>
#include <folly/fibers/Semaphore.h>

#ifndef _WIN32 // Explained in SafeTask.h

/// The `collectAllWindowedAsync()` utility was found "in the wild", and
/// came in a couple of flavors:
///  - A 15-line "naive" version similar in flow to the below, but without
///    `coro/safe`, exhibiting a variety of exception-safety and
///    cancellation-correctness bugs.
///    "Example A" below, or https://fburl.com/code/rq4dshu2
///  - A meticulously reviewed & tested `collectAllWindowedAsync` at 42
///    lines and representing unreasonably many hours of iteration & review.
///    "Example B" below, or https://fburl.com/code/bufr4vsb
///
/// This `coro/safe` analog was written in < 15 minutes, and is correct.
/// The take-home message here is that `coro/safe` makes "naive" code safer.
///
/// This is neither a real unit test (the `CO_TEST`s just shows this works),
/// nor a library-grade utility, due to concerns such as:
///  - Right now, this takes a potentially unsafe `AsyncGenerator`, which is
///    internally wrapped in `manual_safe_val()`.  How should this actually
///    integrate with (future) `coro/safe` wrappers for `AsyncGenerator`?
///    As an example of why taking vanilla `AsyncGenerator`s can cause
///    issues -- right now, in the `cancellation` test below, if you move
///    `generateStuckTasks` inline into `stuckCollect`, you'll get an ASAN
///    failure due to a dangling ref.
///  - The cancellation policy should probably not be hardcoded -- right
///    now, the sub-tasks are cancelled iff the "collect" task is cancelled.
///  - A `folly`-grade implementation might want to handle non-`void` tasks.

namespace folly::coro {

using folly::bindings::make_in_place;

auto collectAllWindowedAsync(
    AsyncGenerator<CoCleanupSafeTask<void>&&> generator,
    const size_t concurrency,
    folly::Executor::KeepAlive<> executor) {
  return async_closure(
      [](auto scope, auto gen, auto sem, auto exec) -> ClosureTask<void> {
        while (auto taskRes = co_await gen.get().next()) {
          co_await sem->co_wait();
          scope->with(exec).schedule(async_closure(
              [](auto sem2, auto task) -> ClosureTask<void> {
                SCOPE_EXIT {
                  sem2->signal();
                };
                co_await std::move(task);
              },
              sem,
              std::move(*taskRes)));
        }
      },
      safeAsyncScope<CancelViaParent>(),
      // FIXME: We don't have a `coro/safe` generator yet.  So, as noted in
      // the docblock, the generator is vulnerable to dangling refs.
      manual_safe_val(std::move(generator)),
      as_capture(make_in_place<fibers::Semaphore>(concurrency)),
      std::move(executor));
}

constexpr int kNumTasks = 10;
constexpr int kConcurrency = 5;
static_assert(kNumTasks > kConcurrency, "*Throws tests need this");

using namespace std::chrono_literals;

namespace {

CoCleanupSafeTask<void> co_sampleTask(capture<std::atomic_int&> ctr) {
  (*ctr)++;
  co_return;
}

CoCleanupSafeTask<void> co_errorTask() {
  co_yield co_error(std::runtime_error("Task"));
}

} // namespace

// While reading the tests, you might wonder: Why create an
// `async_closure()` just to own an `atomic_int` that we reference from the
// `CoCleanupSafeTask`s? Let me explain:
//
//   - It's just a way of (statically) guaranteeing that the lifetime
//     of that atomic is a superset of the sub-tasks' lifetimes.
//
//   - The risks that the outer `async_closure` mitigates here are subtle.
//     For example, this sort of code can access invalid memory:
//       CPUThreadPoolExecutor bgExecutor(5);
//       std::atomic_int runCtr;
//       auto collect = collectAllWindowedAsync(...)
//         .scheduleOn(bgExecutor).start();
//       // do some other stuff that may throw
//       co_await std::move(collect);
//     When this throws, `runCtr` will become invalid while the
//     `Task`s that reference it are still running on `bgExecutor`
//
//   - While inadvisable, you could avoid the closure by declaring the
//     atomic on-stack, and passing it down via
//     `manual_safe_ref<safe_alias::async_scope_safe>()`.

CO_TEST(CollectAllWindowedAsync, success) {
  co_await async_closure(
      [](auto runCtr) -> ClosureTask<void> {
        co_await collectAllWindowedAsync(
            [&]() -> AsyncGenerator<CoCleanupSafeTask<void>&&> {
              for (int i = 0; i < kNumTasks; ++i) {
                co_yield co_sampleTask(runCtr);
              }
            }(),
            kConcurrency,
            folly::getGlobalCPUExecutor());
        EXPECT_EQ(kNumTasks, *runCtr);
      },
      as_capture(make_in_place<std::atomic_int>(0)));
}

CO_TEST(CollectAllWindowedAsync, taskThrows) {
  co_await async_closure(
      [](auto runCtr) -> ClosureTask<void> {
        auto collectTry = co_await co_awaitTry(collectAllWindowedAsync(
            [&]() -> AsyncGenerator<CoCleanupSafeTask<void>&&> {
              for (int i = 0; i < kNumTasks; ++i) {
                co_yield co_sampleTask(runCtr);
              }
              co_yield co_errorTask();
            }(),
            kConcurrency,
            folly::getGlobalCPUExecutor()));
        EXPECT_EQ(collectTry.exception().what(), "std::runtime_error: Task");
        EXPECT_GE(*runCtr, kNumTasks - kConcurrency);
      },
      as_capture(make_in_place<std::atomic_int>(0)));
}

CO_TEST(CollectAllWindowedAsync, generatorThrows) {
  co_await async_closure(
      [](auto runCtr) -> ClosureTask<void> {
        auto collectTry = co_await co_awaitTry(collectAllWindowedAsync(
            [&]() -> AsyncGenerator<CoCleanupSafeTask<void>&&> {
              for (int i = 0; i < kNumTasks; ++i) {
                co_yield co_sampleTask(runCtr);
              }
              throw std::runtime_error("Task Gen");
            }(),
            kConcurrency,
            folly::getGlobalCPUExecutor()));
        EXPECT_EQ(
            collectTry.exception().what(), "std::runtime_error: Task Gen");
        EXPECT_GE(*runCtr, kNumTasks - kConcurrency);
      },
      as_capture(make_in_place<std::atomic_int>(0)));
}

CO_TEST(CollectAllWindowedAsync, cancellation) {
  co_await async_closure(
      [](auto runCtr, auto stuckSemaphore) -> ClosureTask<void> {
        auto generateStuckTasks =
            [&]() -> AsyncGenerator<CoCleanupSafeTask<void>&&> {
          for (int i = 0; i < kNumTasks; ++i) {
            // NB: We can't use `auto` here because our `async_closure`
            // lacks an outer coro, and thus takes its args by-value, e.g.
            // `capture<Semaphore>`.  Specifying wrapped refs here uses
            // the implicit val -> ref conversion.  A concise but "less
            // cheap" idea is to use `async_closure_force_outer_coro` above.
            co_yield
                [](capture<std::atomic_int&> ctr,
                   capture<fibers::Semaphore&> sem) -> CoCleanupSafeTask<void> {
                  co_await sem->co_wait();
                  (*ctr)++;
                }(runCtr, stuckSemaphore);
          }
        };
        auto stuckCollect = collectAllWindowedAsync(
            generateStuckTasks(),
            kNumTasks, // let all the tasks run
            folly::getGlobalCPUExecutor());
        EXPECT_THROW(
            co_await timeout(std::move(stuckCollect), 200ms), FutureTimeout);
        EXPECT_EQ(*runCtr, 0);
      },
      as_capture(make_in_place<std::atomic_int>(0)),
      // Blocks all the background tasks
      as_capture(make_in_place<fibers::Semaphore>(0)));
}

} // namespace folly::coro

/////////////////////////////////////////////////
// Non-compiled code examples referenced above //
/////////////////////////////////////////////////

#if 0 // Example A -- naive and buggy / unsafe

folly::coro::Task<void> collectAllWindowedAsync(
    folly::coro::AsyncGenerator<folly::coro::Task<void>&&> generator,
    size_t concurrency,
    folly::IOThreadPoolExecutor& executor) {
  folly::coro::AsyncScope scope{true};
  folly::fibers::Semaphore semaphore{concurrency};
  while (auto task = co_await generator.next()) {
    co_await semaphore.co_wait();
    scope.add(
        folly::coro::co_invoke(
            [&, t = std::move(*task)]() mutable -> folly::coro::Task<void> {
              SCOPE_EXIT {
                semaphore.signal();
              };
              co_await std::move(t);
            })
            .scheduleOn(executor.getEventBase()));
  }
  co_await scope.joinAsync();
}

#elif 0 // Example B -- correct but difficult

folly::coro::Task<void> collectAllWindowedAsync(
    folly::coro::AsyncGenerator<folly::coro::Task<void>&&> generator,
    size_t concurrency,
    folly::Executor::KeepAlive<> executor) {
  folly::Try<folly::Unit> tryExceptionWrapper;
  folly::fibers::Semaphore semaphore{concurrency};
  folly::coro::CancellableAsyncScope scope{
      folly::copy(co_await folly::coro::co_current_cancellation_token),
      /*throwOnJoin*/ true};

  // Pre-allocate scope cleanup & join upfront to avoid potential bad-alloc
  // blowing up later on exit
  auto scopeCleanup = scope.cancelAndJoinAsync();
  auto scopeJoin = scope.joinAsync();

  try {
    while (auto task = co_await generator.next()) {
      co_await semaphore.co_wait();

      // Safe to pass semaphore by ref since it's guaranteed to outlive the task
      // per scopeJoin/scopeCleanup on exit.
      scope.add(
          ([](folly::fibers::Semaphore& sem,
              folly::coro::Task<void> t) mutable -> folly::coro::Task<void> {
            SCOPE_EXIT {
              sem.signal();
            };
            co_await std::move(t);
          }(semaphore, std::move(*task)))
              .scheduleOn(executor));
    }
  } catch (...) {
    tryExceptionWrapper.emplaceException(
        folly::exception_wrapper(std::current_exception()));
  }

  // respective on-error/on-success exits since we can't co_await inside catch{}
  if (tryExceptionWrapper.hasException()) {
    co_await std::move(scopeCleanup);
    co_yield folly::coro::co_error(tryExceptionWrapper.exception());
  } else {
    co_await std::move(scopeJoin);
  }
}

#endif

#endif
