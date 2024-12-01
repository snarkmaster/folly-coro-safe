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

#pragma once

#include <folly/coro/TaskWrapper.h>

// It looks like some in-use versions of MSVC don't support non-copiable
// coroutines at all. Example errors:
//   NowTaskTest.cpp:23:14: error: call to implicitly-deleted copy
//                          constructor of 'NowTask<int>'
//   NowTask<int> leetTask(int x) {
//                ^~~~~~~~
#ifndef _WIN32

namespace folly::coro {

template <typename>
struct NowTask;

namespace detail {
template <typename T>
class NowTaskPromise final
    : public TaskPromiseWrapper<T, NowTask<T>, TaskPromise<T>> {};
} // namespace detail

/// `NowTask<T>` quacks like `Task<T>` but is nonmovable, and therefore
/// must be `co_await`ed in the same expression that created it.
///
/// Defaulting to `NowTask` brings considerable safety benefits.  With
/// `Task`, the following would be anti-patterns that cause dangling
/// reference bugs, but with `NowTask`, C++ lifetime extension rules ensure
/// that they simply work.
///   - Pass-by-reference into coroutines.
///   - Ephemeral coro lambdas with captures.
///   - Coro lambdas with capture-by-reference.
///
/// Notes:
///   - (subject to change) Unlike `SafeTask`, `NowTask` does NOT check
///     `safe_alias_of_v` for the return type `T`.  The rationale is that
///     `NowTask` is essentially an immediate async function, i.e. it
///     satisfies the structured concurrency maxim of "lexical scope drives
///     both control flow & lifetime".  That shrinks the odds that returned
///     pointers/references are unexpectedly invalid.  The one failure mode
///     I can think of is that the pointed-to-data gets invalidated by a
///     concurrent thread of execution, but in that case the program almost
///     certainly has a data race -- regardless of the lifetime bug -- and
///     that requires runtime instrumentation (like TSAN) to detect in
///     present-day C++.
///   - Even though it can contain unsafe refs, `NowTask` does not need to
///     specialize `safe_alias_for_` -- it's non-movable!
template <typename T>
struct FOLLY_CORO_TASK_ATTRS NowTask final
    : public TaskWrapperCrtp<NowTask<T>, T, Task<T>>,
      private NonCopyableNonMovable {
  using promise_type = detail::NowTaskPromise<T>;
  using TaskWrapperCrtp<NowTask<T>, T, Task<T>>::TaskWrapperCrtp;
};

} // namespace folly::coro

#endif
