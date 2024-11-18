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

/*
XXX add support for capture_unique, use that in SafeAsyncScopeTest

XXX doc

XXX add the `run_` one on-stack -- could have three flavors:
 - after_cleanup(capture_ref) >= body_only_ref, NOT co_cleanup_capture
 - run_after_cleanup(fn, refs as above, no storage, no _cleanup; can move values
into) how to avoid muddling "cleanup refs"?
 - await_after_cleanup

exception safety is "let it fly", right?
*/

namespace folly::coro {

template <typename V, typename ConvertTo = V>
class move_after_cleanup {
 public:
  template <typename CaptureRef>
  move_after_cleanup(CaptureRef ar)
    requires(
        // A "cleanup" arg is invalid now, even if it's `body_only_ref`.
        // `remove_cvref_t` is just paranoia in case of future changes.
        !detail::is_any_co_cleanup_capture<std::remove_cvref_t<CaptureRef>> &&
        // A non-"cleanup" arg only has `body_only_ref` safety if it refers to
        // something owned by the current closure, or its parent, in both
        // cases due to the `shared_cleanup` safety downgrade.
        safe_alias_of_v<CaptureRef> >= safe_alias::body_only_ref)
      : ref_(std::move(*ar)) {
    // Any "cleanup" closure would have an outer coro, and so wouldn't be
    // given any `*capture*<Val>`s.
    static_assert(std::is_reference_v<typename CaptureRef::capture_type>);
  }
  ConvertTo result_after_cleanup(async_closure_private_t) && {
    return std::move(ref_);
  }

 private:
  V&& ref_;
};
template <typename CaptureRef>
move_after_cleanup(CaptureRef)
    -> move_after_cleanup<
        std::remove_reference_t<decltype(*std::declval<CaptureRef>())>>;

} // namespace folly::coro
