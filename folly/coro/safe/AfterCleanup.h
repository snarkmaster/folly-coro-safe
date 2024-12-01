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

#include <folly/coro/safe/AsyncClosure-fwd.h>
#include <folly/coro/safe/Captures.h>

#ifndef _WIN32 // Explained in SafeTask.h

/*
XXX doc

XXX add the `run_` one on-stack -- could have three flavors:
 - after_cleanup(capture_ref) >= after_cleanup_ref, NOT co_cleanup_capture
 - run_after_cleanup(fn, refs as above, no storage, no _cleanup; can move values
into) how to avoid muddling "cleanup refs"?
 - await_after_cleanup

exception safety is "let it fly", right?
*/

namespace folly::coro {

template <typename V, typename ConvertTo = V>
class move_after_cleanup_t {
 public:
  template <typename CaptureRef>
  move_after_cleanup_t(CaptureRef cr)
    requires(
        // Only `capture<Ref>` works here.  Since this closs stores a ref past
        // the end of the scope, local-scope vars would definitely be invalid.
        // As for `capture<Val>` types, `move_after_cleanup*` has a separate
        // overload, since those signal the absence of an outer coro.
        detail::is_any_capture_ref<std::remove_cvref_t<CaptureRef>> &&
        // `co_cleanup` captures aren't allowed regardless of safety (e.g.
        // restricted refs are `after_cleanup_ref`-safe), since they're not
        // meant to be moved.  The `remove_cvref_t` is just paranoia in case
        // of future changes.
        !detail::is_any_co_cleanup_capture<std::remove_cvref_t<CaptureRef>> &&
        // A non-"cleanup" arg only has `after_cleanup_ref` safety if it refers
        // to something owned by the current closure, or its parent, in both
        // cases due to the `shared_cleanup` safety downgrade.
        safe_alias_of_v<CaptureRef> >= safe_alias::after_cleanup_ref)
      : ref_(std::move(*cr)) {
    // Any "cleanup" closure would have an outer coro, and so wouldn't be
    // given any `*capture*<Val>`s.
    static_assert(std::is_reference_v<typename CaptureRef::capture_type>);
  }
  auto result_after_cleanup(async_closure_private_t) && {
    return static_cast<ConvertTo>(std::move(ref_));
  }

 private:
  V&& ref_;
};

// The `capture<Val>` branches below exist so that `move_after_cleanup*`
// works similarly whether or not the closure has an outer coro.
// Unfortunately, I don't know how to make them completely identical -- the
// inner coro return type is `ClosureTask<move_after_cleanup_t<T>>` with an
// outer coro, and `ClosureTask<T>` without.

template <typename CaptureRef>
auto move_after_cleanup(CaptureRef&& c) {
  if constexpr (detail::is_any_capture_ref<std::remove_cvref_t<CaptureRef>>) {
    return move_after_cleanup_t<std::remove_reference_t<decltype(*c)>>{
        std::move(c)};
  } else {
    static_assert(
        detail::is_any_capture_val<std::remove_cvref_t<CaptureRef>>,
        "move_after_cleanup may only take a `capture` type");
    return *std::move(c);
  }
}

template <typename ConvertTo, typename CaptureRef>
auto move_after_cleanup_as(CaptureRef&& c) {
  if constexpr (detail::is_any_capture_ref<std::remove_cvref_t<CaptureRef>>) {
    return move_after_cleanup_t<
        std::remove_reference_t<decltype(*c)>,
        ConvertTo>{std::move(c)};
  } else {
    static_assert(
        detail::is_any_capture_val<std::remove_cvref_t<CaptureRef>>,
        "move_after_cleanup_as may only take a `capture` type");
    return static_cast<ConvertTo>(*std::move(c));
  }
}

} // namespace folly::coro

#endif
