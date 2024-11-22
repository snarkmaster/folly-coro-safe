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

#include <folly/Traits.h>

#include <type_traits>

namespace folly {
template <typename>
class rvalue_reference_wrapper;
} // namespace folly

/*
"Aliasing" is indirect access to memory via pointers or references.  It is
the major cause of memory-safety bugs in C++, but is also essential for
writing correct & performant C++ programs.  Fortunately,
  - Much business logic can be written in a pure-functional style, where
    only value semantics are allowed.  Such code is easier to understand,
    and has much better memory-safety.
  - When references ARE used, the most common scenario is passing a
    reference from a parent lexical scope to descendant scopes.

`safe_alias_of_v` is a _heuristic_ to check whether a type is likely to be
memory-safe in the above settings.  The `safe_alias` enum shows a hierarchy
of memory safety, but you only need to know about two:
  - `unsafe` -- e.g. raw pointers or references, and
  - `maybe_value` -- `int`, `std::pair<int, char>`, or `std::unique_ptr<Foo>`.

A user can easily bypass the heuristic -- since C++ lacks full reflection,
it is impossible to make this bulletproof.  Our goals are much more modest:
  - Make unsafe aliasing **more** visible in code review, and
  - Encourage programmers to use safe semantics by default.

The BIG CAVEATS are:
 - Aliases hidden in structures: We can't see unsafe class members, so
   `UnsafeStruct` below will be deduced to have `maybe_value` safety unless
   you specialize `safe_alias_for_<UnsafeStruct>`.
     struct UnsafeStruct { int* rawPtr; };
 - The "callable hole": If an ancestor scope passes a callable to a child,
   we can't inspect the callable's arg, and so the child can easily pass a
   soon-to-be-dangling reference back to the parent. E.g. this compiles:
       int* badPtr;
       async_closure(
         [](auto fn) -> ClosureTask<void> {
           int i = 5;
           fn(i);
           co_return;
         },
         // We can't tell this callable is unsafe!
         [&](int* p) { badPtr = p; });

If you need to bypass this control, prefer the `manual_safe_*` wrappers
below, instead of writing a custom workaround.  Always explain why it's safe.

You can teach `safe_alias_of_v` about your type by specializing
`folly::detail::safe_alias_for_`, as seen below.  Best practices:
  - Declare the specialization in the same header that declares your type.
  - Only use `maybe_value` if your type ACTUALLY follows value semantics.
  - Use `detail::least_safe_alias()` to aggregate safety across pieces of a
    composite type.
  - Unless you're implementing an `async_closure`-integrated type, it is VERY
    unlikely that you should use `safe_alias::*_cleanup`.
*/
namespace folly {

// ENUM ORDER IS IMPORTANT!  Categories go from "least safe to most safe".
// Always use >= for safety gating.
//
// Note: Only `async_closure*()` from `folly coro/safe/` use the middle
// safety levels `shared_cleanup`, `body_only_ref`, and `co_cleanup_safe_ref`.
// Normal user code should stick to `maybe_value` and `unsafe`.
enum class safe_alias {
  // Definitely has aliasing, we know nothing of the lifetime.
  unsafe,
  // Implementation detail of `async_closure()`, used for creating
  // `ClosureTask`, a restricted-usage `SafeTask`.  Other code should treat
  // this as `unsafe`.  `SafeTask.h` & `Captures.h` explain the rationale.
  unsafe_closure_internal,
  // Implementation detail of `async_closure()`, used for creating
  // `MemberTask`, a restricted-usage `SafeTask`.  Other code should treat
  // this as `unsafe`.  Closure-related code that distinguishes this from
  // `unsafe_closure_internal` expects this value to be higher.
  unsafe_member_internal,
  // Used only in `async_closure*()` -- the minimum level it considers safe
  // for arguments, and the minimum level of `SafeTask` it will emit.
  //   - Represents an arg that can schedule a cleanup callback on an
  //     ancestor's cleanup arg `A`.  This safety level cannot be stronger
  //     than `body_only_ref` because otherwise such a ref could be passed to
  //     a cleanup callback on a different ancestor's cleanup arg `B` -- and
  //     `A` could be invalid by the time `B` runs.
  //   - Follows all the rules of `body_only_ref`.
  //   - Additionally, when a `shared_cleanup` ref is passed to
  //     `async_closure()`, it knows to mark its own args as `body_only_ref`.
  //     This prevents the closure from passing its short-lived `capture`s
  //     into a new callback on the longer-lived `shared_cleanup` arg.
  //     Conversely, in the absence of `shared_cleanup` args, it is safe for
  //     `async_closure()` to upgrade `body_only_capture*<Ref>`s to
  //     `capture*<Ref>`s, since its cleanup will terminate before the
  //     parent's will start.
  shared_cleanup,
  // `async_closure()` won't take `unsafe*` args.  It is important that we
  // disallow `unsafe_closure_internal` in particular, since this is part of
  // the `Captures.h` mechanism that discourages moving `async_closure`
  // capture wrappers out of the closure that owns it (we can't prevent it).
  closure_min_arg_safety = shared_cleanup,
  // Used only in `async_closure*()`:
  //   - Valid ONLY until the current closure's cleanup starts.
  //   - ONLY safe to pass to sub-closures
  //   - NOT safe to return or pass to callbacks from ancestor closures
  //   - NOT safe to pass to callbacks of the same closure's cleanup args
  //     (e.g. `SafeAsyncScope`).
  body_only_ref,
  // Used only in `async_closure*()`:
  //   - Valid until the end of the current closure's cleanup.
  //   - Safe to pass to sub-closures, or to callbacks from this
  //     closure's `SafeAsyncScope` (& other `co_cleanup()` args).
  //   - NOT safe to return or pass to callbacks from ancestor closures.
  co_cleanup_safe_ref,
  // Looks like a "value", i.e. alive as long as you hold it.  Remember
  // this is just a HEURISTIC -- a ref inside a struct will fool it.
  maybe_value
};

template <safe_alias Safety>
struct safe_alias_constant {
  static constexpr safe_alias value = Safety;
};

namespace detail {
// Use only `safe_alias_of_v`, which removes CV qualifiers before testing.
template <typename T>
struct safe_alias_for_ : std::conditional_t<
                             std::is_reference_v<T> || std::is_pointer_v<T>,
                             safe_alias_constant<safe_alias::unsafe>,
                             safe_alias_constant<safe_alias::maybe_value>> {};
template <typename T>
struct safe_alias_for_<std::reference_wrapper<T>>
    : safe_alias_constant<safe_alias::unsafe> {};
template <typename T>
struct safe_alias_for_<folly::rvalue_reference_wrapper<T>>
    : safe_alias_constant<safe_alias::unsafe> {};
} // namespace detail

template <typename T>
inline constexpr safe_alias safe_alias_of_v =
    detail::safe_alias_for_<std::remove_cv_t<T>>::value;

namespace detail {

template <auto FirstV, auto... Vs>
constexpr auto vtag_min(vtag_t<FirstV, Vs...>) {
  auto min_v = FirstV;
  (
      [&]() {
        if (Vs < min_v) {
          min_v = Vs;
        }
      }(),
      ...);
  return min_v;
}

template <safe_alias... Vs>
constexpr safe_alias least_safe_alias(vtag_t<Vs...>) {
  return vtag_min(vtag<safe_alias::maybe_value, Vs...>);
}

// Helper: Inspects its own template args for aliasing.
template <typename... Ts>
struct safe_alias_for_pack_ {
  static constexpr auto value = least_safe_alias(vtag<safe_alias_of_v<Ts>...>);
};

// Let `safe_alias_of_v` recursively inspect `std` containers that
// likely to be involved in bugs.  If you encounter a memory-safety issue
// that would've been caught by this, feel free to extend this.
template <typename... As>
struct safe_alias_for_<std::tuple<As...>> : safe_alias_for_pack_<As...> {};
template <typename... As>
struct safe_alias_for_<std::pair<As...>> : safe_alias_for_pack_<As...> {};
template <typename... As>
struct safe_alias_for_<std::vector<As...>> : safe_alias_for_pack_<As...> {};

// Recursing into `tag_t<>` type lists is nice for metaprogramming
template <typename... As>
struct safe_alias_for_<::folly::tag_t<As...>> : safe_alias_for_pack_<As...> {};

} // namespace detail

// IMPORTANT: If you use the `manual_safe_` escape-hatch wrappers, you MUST
// comment with clear proof of WHY your usage is safe.  The goal is to
// ensure careful review of such code.
//
// Careful: With the default `Safety`, the contained value or reference can
// be passed anywhere -- the wrapper pretends to be value type.
//
// If you know a more restrictive safety level for your ref, annotate it to
// improve safety:
//  - `body_only_ref` for things owned by co_cleanup args of this closure,
//  - `co_cleanup_safe_ref` for refs to non-cleanup args owned by this closure,
//    or any ancestor closure.
//
// The types are public since they may occur in user-facing signatures.

template <safe_alias, typename T>
struct manual_safe_ref_t : std::reference_wrapper<T> {
  using typename std::reference_wrapper<T>::type;
  using std::reference_wrapper<T>::reference_wrapper;
};

template <safe_alias, typename T>
struct manual_safe_val_t {
  using type = T;

  template <typename... Args>
  manual_safe_val_t(Args&&... args) : t_(std::forward<Args>(args)...) {}

  T& get() noexcept { return t_; }
  operator T&() noexcept { return t_; }
  const T& get() const noexcept { return t_; }
  operator const T&() const noexcept { return t_; }

 private:
  T t_;
};

template <safe_alias Safety = safe_alias::maybe_value, typename T = void>
inline auto manual_safe_ref(T& t) {
  return manual_safe_ref_t<Safety, T>{t};
}
template <safe_alias Safety = safe_alias::maybe_value>
inline auto manual_safe_val(auto t) {
  return manual_safe_val_t<Safety, decltype(t)>{std::move(t)};
}

namespace detail {
template <safe_alias S, typename T>
struct safe_alias_for_<manual_safe_ref_t<S, T>> : safe_alias_constant<S> {};
template <safe_alias S, typename T>
struct safe_alias_for_<manual_safe_val_t<S, T>> : safe_alias_constant<S> {};
} // namespace detail

// Use `SafeTask<>` instead of `Task` to move tasks into other safe coro APIs.
//
// User-facing stuff from `Task.h` can trivially include unsafe aliasing,
// the `folly::coro` docs include hundreds of words of pitfalls.  The intent
// here is to catch people accidentally passing `Task`s into safer
// primitives, and breaking their memory-safety guarantess.
//
// Future: Move this into `Task.h` once `SafeAlias.h` is an established pattern.
namespace coro {
template <typename T>
class TaskWithExecutor;
template <typename T>
class Task;
} // namespace coro
namespace detail {
template <typename T>
struct safe_alias_for_<::folly::coro::TaskWithExecutor<T>>
    : safe_alias_constant<safe_alias::unsafe> {};
template <typename T>
struct safe_alias_for_<::folly::coro::Task<T>>
    : safe_alias_constant<safe_alias::unsafe> {};
} // namespace detail

// Future: Implement a `coro/safe` wrapper generator wrapper.
// Future: This `safe_alias_for_` should sit in `AsyncGenerator.h`
// once `SafeAlias.h` is an established pattern.
namespace coro {
template <typename, typename, bool>
class AsyncGenerator;
} // namespace coro
namespace detail {
template <typename Ref, typename Val, bool Clean>
struct safe_alias_for_<::folly::coro::AsyncGenerator<Ref, Val, Clean>>
    : safe_alias_constant<safe_alias::unsafe> {};
} // namespace detail

} // namespace folly
