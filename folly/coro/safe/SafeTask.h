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
#include <folly/coro/safe/NowTask.h>
#include <folly/coro/safe/SafeAlias.h>

// `folly/coro/safe` is, by and large, C++20 code.  Unfortunately, the rest
// of `folly` still has to support some older C++17-only versions of MSVC
// (which also include "fun" compiler bugs).  Rather than fight with these,
// the plan is to focus on adoption of `folly/coro/safe` on Linux & Mac, and
// wait for the older MSVCs to age out.
#ifndef _WIN32

namespace folly::coro {

/// Typically, you will not use `SafeTask` directly.  Instead, choose one of
/// the type-aliases below, following `APIBestPractices.md` guidance.  Briefly:
///   - `ValueTask`: Use if your coro only takes value-semantic args.
///   - `MemberTask`: Use for all non-static member functions.  Can be
///     awaited immediately (like `NowTask`), or wrapped in an
///     `async_closure()` to support less-structured concurrency --
///     including scheduling on a background scope belonging to the object.
///   - `ClosureTask`: Use if your coro is wrapped by `as_async_closure()` or
///     otherwise called via `async_closure()`.
///   - `CoCleanupSafeTask`: Use for tasks that can be directly scheduled on a
///     `SafeAsyncScope`.
///   - (not in `SafeTask.h`) `NowTask`: All other coros.  This requires the
///     task to always be awaited in the expression that created it,
///     eliminating a variety of common dangling reference bugs.
///   - `AutoSafeTask`: Generic coros where you want the argument & return
///     types to automatically branch between a `NowTask` and a `SafeTask`.
///
/// `SafeTask` is a thin wrapper around `folly::coro::Task` that uses
/// `safe_alias_of_v` to enforce some compile-time guarantees:
///   - The `safe_alias_of_v` memory safety of the coro's arguments
///     is at least as high as declared.
///   - Regardless of the declared safety, the coro's return must have
///     safety `maybe_value` (explained in `SafeTaskRetAndArgs`).
///   - The coroutine is NOT a stateful callable -- this prohibits lambda
///     captures, since those a very common cause of coro memory bugs.
template <safe_alias, typename>
class SafeTask;

// A `SafeTask` whose args and return type follow value semantics.
template <typename T>
using ValueTask = SafeTask<safe_alias::maybe_value, T>;

// A `SafeTask` that can be added to `SafeAsyncScope`, and may run during
// closure cleanup.  Its content must therefore be `co_cleanup_safe_ref`-safe.
template <typename T>
using CoCleanupSafeTask = SafeTask<safe_alias::co_cleanup_safe_ref, T>;

// Use `ClosureTask` as the inner coro type for tasks meant to be wrapped
// in an `async_closure` (`as_async_closure`, `schedule*Closure`, etc).
//
// Outside of a closure, `ClosureTask` is neither awaitable nor movable.
// The `unsafe_closure_internal` specialization below explains why.
//
// If your use-case calls for a `SafeTask` that is sometimes wrapped in a
// closure, and sometimes isn't, you might add a `MinClosureSafeTask` type
// alias for `closure_min_arg_safety`.
template <typename T>
using ClosureTask = SafeTask<safe_alias::unsafe_closure_internal, T>;

// A `MemberTask` is a hybrid of `SafeTask` and `NowTask`, intended to make
// non-static member coroutines safer.
//   - It **is** a `SafeTask`, thereby forbidding `safe_alias::unsafe`
//     arguments, and unsafe return types.  However, since the callable of
//     member coros is inherently stateful, it is special-cased to omit the
//     safety checks on the implicit object parameter.
//   - It is non-movable like `NowTask`, which makes typical "structured
//     concurrency" usage of coroutines quite safe (see `NowTask.h`).
//
// For more complex usage (background tasks, async RAII), `MemberTask` has a
// special calling convention in `AsyncClosure.h`:
//
//   async_closure(FOLLY_INVOKE_MEMBER(memberTaskName), obj, args...)
//
// Like any async closure, this safety-checks the now-explicit object param,
// and produces a movable `SafeTask` of the safety level determined from the
// arguments.  This integration lets us safely schedule member coros on
// `SafeAsyncScope`, pass `co_cleanup` args into such coros, etc.
template <typename T>
using MemberTask = SafeTask<safe_alias::unsafe_member_internal, T>;

// NB: There are some `async_closure()`-specific values of `safe_alias` that
// do not yet have a `SafeTask` alias.  That's because they haven't come up
// in user-facing type signatures.

namespace detail {
template <typename T, safe_alias Safety>
using AutoSafeTaskImpl = std::conditional_t<
    // This checks both args & the return value because we want to avoid this
    // resolving to a `SafeTask` that won't actually compile.
    (Safety >= safe_alias::closure_min_arg_safety &&
     safe_alias_of_v<T> >= safe_alias::maybe_value),
    SafeTask<Safety, T>,
    NowTask<T>>;
}

/// Coros declared as `SafeTask<Safety, T>` will satisfy the strong
/// constraints above, or fail with a compile error.
///
/// The safety of a coroutine template may vary depending on the args or
/// return type, meaning that the user can't actually pick a fixed
/// safety level for their generic coro.
///
/// Instead, the generic coro can return `AutoSafeTask<ReturnT,
/// SafetyArgs...>`, where `SafetyArgs` is (typically) the subset of the
/// coroutine's argument types that may affect safety.
///
/// `AutoSafeTask` has a Significant Caveat -- you can't use it with
/// non-`static` member functions -- the implicit object parameter is unsafe
/// (as it should be).  And if you do use it, you will get a compile-time
/// error instead of a `NowTask`, simply because this type-function has no
/// access to the callable.  See `APIBestPractices.md` for workarounds.
template <typename T, typename... SafetyArgs>
using AutoSafeTask = detail::AutoSafeTaskImpl<
    T,
    ::folly::detail::safe_alias_for_pack_<SafetyArgs...>::value>;

namespace detail {

struct SafeTaskTest;

template <safe_alias ArgSafety, typename RetT, typename... Args>
concept SafeTaskRetAndArgs = ((safe_alias_of_v<Args> >= ArgSafety) && ...) &&
    // In the event that you need a child scope to return a reference to
    // something owned by a still-valid ancestor scope, we don't have a good
    // way to detect this automatically.  To work around, use a `manual_safe_*`
    // wrapper in `SafeAlias.h`, and comment why it is safe.
    (safe_alias_of_v<RetT> >= safe_alias::maybe_value);

template <typename T>
concept is_stateless_class_or_func =
    (std::is_class_v<T> && std::is_empty_v<T>) ||
    (std::is_pointer_v<T> && std::is_function_v<std::remove_pointer_t<T>>);

template <safe_alias, typename...>
inline constexpr bool IsSafeTaskValid = false;
// Coros taking 0 args can't be methods (no implicit object parameter),
// so their safety is determined by the return type.
template <safe_alias ArgSafety, typename RetT>
inline constexpr bool IsSafeTaskValid<ArgSafety, RetT> =
    SafeTaskRetAndArgs<ArgSafety, RetT>;
// Inspect the first argument, which can be an implicit object parameter, to
// allow stateless callables (like lambdas), but to prohibit stateful
// callables (these can contain unsafe aliasing in their state, which we
// can't inspect).  If you need to make `SafeTask`s from a stateful object,
// pass `capture<Ref>` to a static func, and check out `AsyncObject.h`.
//
// How this works: With >= 1 args in the pack, the `First` argument
// **could** be an implicit object parameter.  We don't know if it is, but
// we do know that any such parameter has type lvalue reference, which means
// that it would fail `SafeTaskRetAndArgs<RetT, First, Args...>`.
//
// This test accepts any `First` that is an lref to a stateless class
// or function -- that is, it returns `true` if the first arg is either:
//   - not an lref, and has no unsafe aliasing (not an implicit object param)
//   - an lref to something stateless (MAY be an implicit object param)
// As a side effect, this allows coros without an implicit object param to
// pass a stateless class by reference, if it's the first param.  This
// should be harmless in practice.
//
// FIXME: It should (?) be fine to simplify this scenario by having
// `SafeAlias` mark as "safe" all references-to-empty-classes, and all
// function pointers.  Then, only a shortened comment would survive.
//
// For `MemeberTask`, `First` is assumed to be the implicit object
// parameter.  This cannot be safe, so we don't check it, and instead rely
// on `MemberTask`'s usage restrictions (see also `SafeTaskBaseTrait`).
template <safe_alias ArgSafety, typename RetT, typename First, typename... Args>
inline constexpr bool IsSafeTaskValid<ArgSafety, RetT, First, Args...> =
    ((ArgSafety == safe_alias::unsafe_member_internal) ||
     (std::is_lvalue_reference_v<First> &&
      is_stateless_class_or_func<std::remove_reference_t<First>>))
    ? SafeTaskRetAndArgs<ArgSafety, RetT, Args...>
    : SafeTaskRetAndArgs<ArgSafety, RetT, First, Args...>;

template <safe_alias ArgSafety, typename T, typename... Args>
class SafeTaskPromise final : public TaskPromiseWrapper<
                                  T,
                                  SafeTask<ArgSafety, T>,
                                  detail::TaskPromise<T>> {
  // "Unsafe" is not much of a "safe" task any more...
  static_assert(ArgSafety > safe_alias::unsafe);

 public:
  // IMPORTANT: If you alter this arrangement, do the "Manual test" inside
  // `returnsVoid` in `SafeTaskTest.cpp`.
  //
  // This is a no-op wrapper.  It needs to exist because `IsSafeTaskValid`
  // requires the coroutine function to be a complete type before checking
  // if it's a stateless callable, and the easiest place to do that is in a
  // class function that's guaranteed to be instantiated, such as this.
  SafeTask<ArgSafety, T> get_return_object() noexcept {
    // If your build failed here, your `SafeTask<>` coro declaration is
    // invalid.  Specific causes for this failure:
    //   - One of the arguments, or the return value, contains "unsafe
    //     aliasing" -- see `SafeAlias.h` for the details.  Typical
    //     causes include raw pointers, references, reference wrappers, etc.
    //   - A stateful callable: lambda with captures, class with members, etc.
    static_assert(
        detail::IsSafeTaskValid<ArgSafety, T, Args...>,
        "Bad SafeTask: check for unsafe aliasing in arguments or return "
        "type; also ensure your callable is stateless.");
    return TaskPromiseWrapper<
        T,
        SafeTask<ArgSafety, T>,
        detail::TaskPromise<T>>::get_return_object();
  }
};

template <bool>
auto bind_captures_to_closure(auto, auto&&...);

template <safe_alias ArgSafety, typename T>
struct SafeTaskBaseTraits {
  using type = TaskWrapperCrtp<SafeTask<ArgSafety, T>, T, Task<T>>;
};

// `async_closure` cannot emit a `SafeTask` without the inner coro being a
// `SafeTask` -- otherwise it could not guarantee that none of the args are
// taken by reference.  Conveniently, `SafeTask` also checks the return type
// is safe, and the coro's callable is stateless, so `async_closure` can
// skip those checks.
//
// This `ClosureTask` implementation uses a `safe_alias` level safer than
// `unsafe` to get all of the above `SafeAlias` checks.  The level also has
// to be less safe than `shared_cleanup` so we can treat these differently:
//  - `capture<Value>` (safety `unsafe_closure_internal`) should stay in the
//    original closure.  Users can move the content, but shouldn't move the
//    wrapper, since that messes with the safety system.
//  - `co_cleanup_capture<Value&>` refs (safety `shared_cleanup`) can
//    safetly be moved or copied into other closures.
//  - By the way, `co_cleanup_capture<Value>` should never be moved from the
//    owning closure that's responsible for its cleanup.
template <typename T>
struct SafeTaskBaseTraits<safe_alias::unsafe_closure_internal, T> {
  using base = OpaqueTaskWrapperCrtp<
      SafeTask<safe_alias::unsafe_closure_internal, T>,
      T,
      Task<T>>;
  // In today's usage, `ClosureTask` does not benefit from being movable, so
  // mark it non-movable to be safer & preserve option value.
  struct type : public base, private NonCopyableNonMovable {
    using base::base;
  };
};

// `MemberTask` implementation, also based on a special `safe_alias` level
// similar to `ClosureTask`.
template <typename T>
struct SafeTaskBaseTraits<safe_alias::unsafe_member_internal, T> {
  // Unlike `ClosureTask`, this **is** awaitable outside of `async_closure`,
  // and therefore it **must** be non-movable to mitigate safety risks.
  using base = TaskWrapperCrtp<
      SafeTask<safe_alias::unsafe_member_internal, T>,
      T,
      Task<T>>;
  struct type : public base, private NonCopyableNonMovable {
    using base::base;
  };
};

template <typename>
struct safe_task_traits {
  static constexpr safe_alias arg_safety = safe_alias::unsafe;
  // NB: No `return_type` since it's not a `SafeTask`.
};

template <safe_alias ArgSafety, typename T>
struct safe_task_traits<SafeTask<ArgSafety, T>> {
  static constexpr safe_alias arg_safety = ArgSafety;
  using return_type = T;
};

} // namespace detail

// This wrapper around `TaskWithExecutor<T>` is very thin because we
// currently only need it for `SafeAsyncScope`, which can just `unwrap()`
// after validating safety.  Forward more functions if needed.
template <safe_alias ArgSafety, typename T>
class FOLLY_NODISCARD SafeTaskWithExecutor final {
 public:
  TaskWithExecutor<T> unwrap() && { return std::move(inner_); }

 protected:
  template <safe_alias, typename>
  friend class SafeTask;

  explicit SafeTaskWithExecutor(TaskWithExecutor<T> t) : inner_(std::move(t)) {}

 private:
  TaskWithExecutor<T> inner_;
};

template <safe_alias ArgSafety, typename T>
class FOLLY_CORO_TASK_ATTRS SafeTask final
    : public detail::SafeTaskBaseTraits<ArgSafety, T>::type {
 protected:
  friend struct folly::coro::detail::SafeTaskTest;
  template <safe_alias, typename>
  friend class SafeTask; // `withNewSafety` makes a different `SafeTask`
  template <bool> // uses `withNewSafety`
  friend auto detail::bind_captures_to_closure(auto, auto&&...);

  // The `async_closure()` implementation is allowed to override the
  // argument-deduced `safe_alias_of_v` for a `SafeTask` because
  // `capture_safety` marks some coro-stored `*capture*`s as `unsafe`
  // even though they're safe -- to discourage users from moving them.
  template <safe_alias NewSafety>
  SafeTask<NewSafety, T> withNewSafety() && {
    return SafeTask<NewSafety, T>{std::move(*this).unwrap()};
  }

 public:
  // There is no `promise_type` here because it's added by `coroutine_traits`
  // below.  This is the mechanism that enables `SafeTaskPromise` to inspect
  // the specific arguments of the coroutine (including the implicit object
  // parameter), and fail the compilation if anything looks unsafe.
  using detail::SafeTaskBaseTraits<ArgSafety, T>::type::type;

  // The main use-case for this is if you want to set your own executor
  // before passing a `Task` to a `SafeAsyncScope`.
  FOLLY_NODISCARD
  SafeTaskWithExecutor<ArgSafety, T> scheduleOnSafe(
      Executor::KeepAlive<> executor) && noexcept {
    return SafeTaskWithExecutor<ArgSafety, T>{
        std::move(*this).unwrap().scheduleOn(std::move(executor))};
  }
};

} // namespace folly::coro

template <folly::safe_alias ArgSafety, typename T, typename... Args>
struct folly::coro::
    coroutine_traits<folly::coro::SafeTask<ArgSafety, T>, Args...> {
  // UGH: Pass `Args...` into `SafeTaskPromise` because at this point, the
  // coroutine function is still an incomplete type, and can't be validated.
  using promise_type =
      folly::coro::detail::SafeTaskPromise<ArgSafety, T, Args...>;
};

// For `safe_alias_of_v`: use the task while its arguments are still good.
template <folly::safe_alias ArgSafety, typename T>
struct folly::detail::safe_alias_for_<folly::coro::SafeTask<ArgSafety, T>>
    : folly::safe_alias_constant<ArgSafety> {};
template <folly::safe_alias ArgSafety, typename T>
struct folly::detail::safe_alias_for_<
    folly::coro::SafeTaskWithExecutor<ArgSafety, T>>
    : folly::safe_alias_constant<ArgSafety> {};

#endif
