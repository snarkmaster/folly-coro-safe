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

#include <folly/portability/SourceLocation.h>

#include <folly/coro/AsyncScope.h>
#include <folly/coro/safe/AsyncClosure-fwd.h>
#include <folly/coro/safe/Captures.h>

#ifndef _WIN32 // Explained in SafeTask.h

/// `SafeAsyncScope` wraps `AsyncScope` to improve safety:
///   - Exception-safety: `SafeAsyncScope` automatically waits to join the
///     scope, while manual `join()` calls often have uncaught exception bugs.
///   - Memory-safety: You get some compile-time protection from dangling
///     reference (lifetime) bug via `SafeAlias.h` checks (see its docs).
///   - Cancellation-safety: The default policy is what most callsites need,
///     and you can easily select or implement other cancellation policies.
///
/// In short, this has good defaults, a simple API (on `SafeAsyncScopeProxy`),
/// and compile-type guards against common bugs.
///
/// `SafeAsyncScope` is meant to be used as an `async_closure()` arg:
///
///    co_await async_closure([](auto scope) -> Task<void> {
///      scope.with(co_await co_current_executor).schedule(async_closure(...));
///    }, safeAsyncScope<CancelViaParent>());
///
/// You will find real examples in `SafeAsyncScopeTest.cpp`.
///
/// ## Cancellation
///
/// Before you add tasks to a scope, DO think about cancellation.
///
/// IMPORTANT: If you call `co_withCancellation` on a task before passing it
/// to the `SafeAsyncScope`, that task will NOT get any cancellation signals
/// through the scope (per-closure, per-scope, or `tokensToMerge`).  This is
/// usually NOT what you want, but in rare cases it can be a way to opt out
/// of the scope's cancellation policy on a one-off basis.
///
/// Here are the cancellation policies -- you can also define your own:
///   - NeverCancel
///   - CancelViaParent
///     * "Parent" will tie cancellation of the scope's tasks to the
///       cancellation of the owning `async_closure` (or destructor scope,
///       for `AsyncObject`).
///   - CancelOnRequest, CancelViaParentOrRequest
///     * "Request" policies create a separate cancellation source just for
///       your scope, and add a `requestCancellation()` method.
///   - CancelOnErrorOrRequest, CancelOnExitOrRequest
///     * These policies' `bool operator()(const exception_wrapper&)` is
///       called with the error of your "inner coro" when the
///       `async_closure` or `AsyncObject` owning this scope is cleaned up.
///       Returning `true` causes `requestCancellation()`.
///       NOTE: You'll never see an error on `AsyncObject` scopes!
///   - Future: CancelViaTokens, CancelViaTokensOrRequest
///     * "Tokens" is an explicit tuple of tokens you pass to the scope's
///       constructor. Recall: you can also set `tokensToMerge` per subtask.
///     * To implement, just add `WithExternalCancelTokens` storing a tuple.
///
/// For most people, `safeAsyncScope<CancelViaParent>()` should work well.
///
/// ## Recursive scheduling
///
/// Scope tasks can schedule more scope tasks.  The good news is that this
/// is safe to do even after cleanup has been started, because `add` remains
/// safe until ALL scope tasks are joined -- and we're inside an active one!
/// The bad news is that we don't support simply passing the outer `scope`
/// ref to the scope task's `async_closure()`, for reasons described in
/// "Scheduling from child closures" below.
///
/// Instead, you have to call `with(executor).scheduleScopeClosure(...)`,
/// which will add a reference the current scope as the first argument.
///
/// ## Scheduling from child closures
///
/// The parent closure owning the `SafeAsyncScope` gets an argument of type
/// `co_cleanup_capture<SafeAsyncScope>`.  This can be passed freely to
/// sub-closures being awaited by the parent -- but NOT to sub-tasks
/// scheduled on the scope.
///
/// There is a CAVEAT -- such sub-closures will see the safety of their own
/// `capture`s downgraded to `after_cleanup_ref`.  This is to prevent this class
/// of dangling reference bug: the sub-closure schedules a sub-task on the
/// scope, referencing its own arg, which could easily be freed before the
/// sub-task completes.
///
/// To avoid the downgrade behavior, use `restricted_co_cleanup_capture<>`.
/// This uses a special `SafeAsyncScopeProxy` that can only schedule
/// `ValueTask` sub-tasks.
///
/// TODO: Finish implementing this.
///
/// ## Why are all public methods on a `Proxy` object?
///
/// Moreover, we DELIBERATELY do not expose a proxy via `SafeAsyncScope`
/// operators like `*` or `->`, despite the 1-line implementation.
///
/// Per the prior section, we need a proxy so that
/// `restricted_co_cleanup_capture` refs to a scope can expose a more limited
/// API than `co_cleanup_capture` refs do.
///
/// We DO NOT expose our API via `SafeAsyncScope` itself because doing so
/// would defeat our main purpose.  Users could then make and use "safe"
/// scopes in any context (not just via `async_closure` or `AsyncObject`s),
/// exposing them to the cleanup & lifetime safety issues we aim to improve.

namespace folly::coro {

template <typename>
class SafeAsyncScopeProxy;

template <typename, size_t>
class SafeAsyncScopeContextProxy;

template <safe_alias, typename>
class SafeTaskWithExecutor;

namespace detail {

template <typename, safe_alias MinSafety, typename Ret>
inline constexpr bool is_safe_task_with_executor_v = false;
template <safe_alias Actual, safe_alias Min, typename R>
inline constexpr bool
    is_safe_task_with_executor_v<SafeTaskWithExecutor<Actual, R>, Min, R> =
        (Actual >= Min);
template <typename T, safe_alias Safety>
concept is_void_safe_task_with_executor =
    is_safe_task_with_executor_v<T, Safety, void>;

// Do NOT expose unsafe methods like "join" or "add" from the underlying
// scope.  The inline comments discuss the necessary constraints.
template <typename Derived>
class SafeAsyncScopeCrtpBase {
 public:
  SafeAsyncScopeCrtpBase() : scope_(/*throwOnJoin*/ true) {}

  // Private, invoked automatically on `async_closure()` exit.
  //
  // IMPORTANT: For now, you cannot await cleanup before the closure ends.
  // This can be solved, but it requires work to address two problems:
  //
  // (1) Deadlocks: Awaiting cleanup from an `add`edd task will deadlock.
  // All `co_cleanup_capture` varieties have safety `< co_cleanup_safe_ref`, so
  // this only affects first-arg refs from `scheduleScopeClosure()`.  To
  // mitigate this risk, we could introduce another `capture` ref variety, which
  // does NOT allow awaiting cleanup -- and have `scheduleScopeClosure()`
  // return that.
  //
  // (2) Thread-safety: Awaiting cleanup while the closure is active would
  // be thread-unsafe wrt `add` / `schedule`.  There are 3 options:
  //   - Easy + unsafe: Add `co_cleanupThreadUnsafe()` to
  //     `SafeAsyncScopeProxy`, and pray.
  //   - Safe + tolerant: Add `EpochSafeAsyncScope*` that enables
  //     `SafeAsyncScopeProxy::co_cleanupEpoch`.  The internal implemenetation
  //     uses `Synchronized<>` (or `ReadMostlyTLObserver<>`) to synchronize
  //     so that all `add` & `schedule`s up to a certain point go to an
  //     `AsyncScope` for an old "epoch" -- and its cleanup happens last.
  //     Tasks added after the old epoch cutoff are scheduled on the next
  //     epoch.  Then, `async_closure()` cleans up the last epoch.
  //   - Safe + strict: Add `EarlyCleanupAsyncScope`, that enables
  //     `SafeAsyncScopeProxy::co_cleanupAndFailFutureSchedules`.  This
  //     internally uses `Synchronized<optional<AsyncScope...>>` to break
  //     any `schedule` requests that arrive after cleanup began.
  Task<void> co_cleanup(
      async_closure_private_t, const exception_wrapper* ep) && {
    return [](auto& me, auto join, auto& ew) -> Task<void> {
      // Future: could have reference objects increment an atomic refcount, in
      // debug builds, and FATAL if that's nonzero here.
      if constexpr (requires { me.enactCancellationPolicy(ew); }) {
        me.enactCancellationPolicy(ew);
      }
      co_await std::move(join);
    }(*static_cast<Derived*>(this), scope_.joinAsync(), *ep);
  }

  friend auto capture_lref_proxy(capture_private_t, Derived& me) {
    return SafeAsyncScopeProxy{me};
  }
  friend auto capture_ptr_proxy(capture_private_t, Derived& me) {
    return SafeAsyncScopeProxy{me};
  }

 protected:
  template <typename>
  friend class folly::coro::SafeAsyncScopeProxy;
  template <typename, size_t>
  friend class folly::coro::SafeAsyncScopeContextProxy;

  // The user docs are on SafeAsyncScopeProxy::add().
  //
  // Implementation notes:
  //   - Call `scheduleOnSafe` on a `ValueTask` or `CoCleanupSafeTask` to get
  //     the awaitable `t`.
  //   - `returnAddress` should be used when `add()` is called from a
  //     non-coro.  Any `add()` wrapper should be `FOLLY_NOINLINE` and default
  //     the address to `FOLLY_ASYNC_STACK_RETURN_ADDRESS()`.
  //   - `source_location` is good for debugging and very cheap.  If you
  //     find a case where it hurts perf, add a location-less overload.
  //   - `tokensToMerge` takes a tuple-like of cancellation tokens, so "you
  //     pay for what you use". `merge()` can be improved to avoid allocation
  //     when only one of the supplied tokens is nontrivial.
  //   - The function is `const` to permit the use of cheap `Synchronized`
  //     reader locks (or other read-mostly patterns) to synchronize cleanup
  //     and scheduling.  Specifically, the underyling `addWithSourceLoc`
  //     must not race with `joinAsync()` -- unless `addWithSourceLoc` is
  //     being called from a task already scheduled on the current
  //     `AsyncScope` (then, any attempt to trigger cleanup will block until
  //     the task finishes).  This means that `add`ed awaitables don't need
  //     to be aware whether cleanup had already begun.  And the `>=
  //     co_cleanup_safe_ref` constraint means that such an awaitable cannot
  //     take a reference to another cleanup arg.
  void add(
      detail::is_void_safe_task_with_executor<
          safe_alias::co_cleanup_safe_ref> auto t,
      auto tokensToMerge,
      source_location sourceLoc,
      void* returnAddress,
      auto cancelTokenSpyFn) const {
    auto& me = *static_cast<const Derived*>(this);
    scope_.addWithSourceLoc(
        std::apply(
            [&](std::same_as<CancellationToken> auto... cts) {
              // Rely on `merge(std::move(x))` being a no-op (D65044520).
              auto ctok = CancellationToken::merge(std::move(cts)...);
              cancelTokenSpyFn(ctok);
              return co_withCancellation(
                  std::move(ctok), std::move(t).unwrap());
            },
            std::tuple_cat(
                std::move(tokensToMerge),
                me.externalCancellationTuple(),
                me.scopeCancellationTuple())),
        returnAddress,
        std::move(sourceLoc));
  }

 private:
  // This is `mutable` so `add()` can be `const` -- read its docblock.
  mutable AsyncScope scope_;
};

// A mixin for `SafeAsyncScope` flavors that provides a scope-internal
// cancellation source.  `enactCancellationPolicy` is called on cleanup.
//
// NB: This replicates `CancellableAsyncScope` functionality, but this
// approach avoids potentially redundant heap allocations from having to
// merge tokens multiple times.
template <typename CancelPolicy>
class WithPerScopeCancellationSource {
 public:
  WithPerScopeCancellationSource() noexcept
      : cancellationToken_(cancellationSource_.getToken()) {}

  void requestCancellation() const noexcept {
    cancellationSource_.requestCancellation();
  }

 protected:
  template <typename>
  friend class SafeAsyncScopeCrtpBase;

  auto scopeCancellationTuple() const { return std::tuple{cancellationToken_}; }

  // MUST NOT THROW.  The `exception_wrapper` is nonempty if the inner coro
  // of the `async_closure` exits with an error.
  void enactCancellationPolicy(const exception_wrapper& ew) {
    if (CancelPolicy{}(ew)) {
      requestCancellation();
    }
  }

 private:
  CancellationSource cancellationSource_;
  CancellationToken cancellationToken_;
};

// Mixin for scopes that don't want cancellation via the parent, or other
// external cancellation tokens.
class WithoutExternalCancelTokens {
 protected:
  template <typename>
  friend class SafeAsyncScopeCrtpBase;

  auto externalCancellationTuple() const { return std::tuple{}; }
};

// Mixin for scopes whose tasks use to the parent coro's cancellation token
// (coro awaiting the `async_closure`, or the dtor scope of an `AsyncObject`).
class WithParentCancelToken {
 public:
  // The `async_closure` and `AsyncObject` implementations look for this
  // special method.
  //
  // Performance note: Right now, we make an extraneous, eager copy of the
  // token.  But, these extra atomic ops are avoidable, because today, all
  // `folly::coro`s store their token on the coroutine promise object,
  // meaning that we could safely store a pointer here instead -- a scope
  // should always be destructed before its parent coro does.  This is not
  // currently done because `scheduleReturningCancelToken` above can't yet
  // get at this pointer.  API proposal: https://fburl.com/ctok_stable_ptr
  void setParentCancelToken(
      async_closure_private_t, const CancellationToken& t) {
    parentCancelToken_ = t;
  }

 protected:
  template <typename>
  friend class SafeAsyncScopeCrtpBase;

  auto externalCancellationTuple() const {
    return std::tuple{parentCancelToken_};
  }

 private:
  CancellationToken parentCancelToken_;
};

} // namespace detail

// Cancel sub-tasks iff the "inner coro" of `async_closure` exits with any
// error.  Same as `CancelOnExit` for scopes inside `AsyncObject`s.
struct CancelOnErrorOrRequest {
  // Why not `WithParent`?  Usually, a closure will exit with
  // `OperationCancelled`, so it's not very useful to also listen to the
  // parent's cancellation token.  It would also be UNEXPECTED if your
  // closure had already exited without an error, and a cancellation on its
  // token canceled pending sub-tasks.  So, if you need this edge-case to
  // behave differently, it should probably be a differently-named policy.
  using ExternalCancelTokenSource = detail::WithoutExternalCancelTokens;
  bool operator()(const exception_wrapper& ew) {
    return ew.has_exception_ptr();
  }
};

// Exiting the closure requests cancellation of sub-tasks.
struct CancelOnExitOrRequest {
  // Like in `CancelOnErrorOrRequest`, we don't need the parent's token.
  using ExternalCancelTokenSource = detail::WithoutExternalCancelTokens;
  std::true_type operator()(const exception_wrapper&) { return {}; }
};

// Only cancel sub-tasks via `requestCancellation()`.
struct CancelOnRequest {
  using ExternalCancelTokenSource = detail::WithoutExternalCancelTokens;
  std::false_type operator()(const exception_wrapper&) { return {}; }
};

// Cancel sub-tasks by canceling the closure, or via `requestCancellation()`.
struct CancelViaParentOrRequest {
  using ExternalCancelTokenSource = detail::WithParentCancelToken;
  std::false_type operator()(const exception_wrapper&) { return {}; }
};

// There are no public methods besides those in the `SafeAsyncScope` body here.
//  - Construct via `safeAsyncScope()`, below.
//  - See `SafeAsyncScopeProxy` for the main API: `add`, `with().schedule`.
//  - Note that `CancelPolicy` does NOT affect sub-tasks which already
//    had `co_withCancellation` called against them.  If you run into this,
//    use the `tokensToMerge` parameter.
template <typename CancelPolicy>
class SafeAsyncScope final //
    : public CancelPolicy::ExternalCancelTokenSource,
      public detail::WithPerScopeCancellationSource<CancelPolicy>,
      public detail::SafeAsyncScopeCrtpBase<SafeAsyncScope<CancelPolicy>> {};

// The added awaitables don't get scope or closure cancellation signals.
// You can still pass per-awaitable `tokensToMerge` (best practice), or
// manually call `co_withCancellation` (avoid).
struct NeverCancel {};
template <>
class SafeAsyncScope<NeverCancel> final
    : public detail::WithoutExternalCancelTokens,
      public detail::SafeAsyncScopeCrtpBase<SafeAsyncScope<NeverCancel>> {
 protected:
  template <typename>
  friend class detail::SafeAsyncScopeCrtpBase;

  auto scopeCancellationTuple() const { return std::tuple{}; }
};

// Default: Added awaitables listen to the closure's cancellation token,
// unless you manually override this via `co_withCancellation`.
struct CancelViaParent {};
template <>
class SafeAsyncScope<CancelViaParent> final
    : public detail::WithParentCancelToken,
      public detail::SafeAsyncScopeCrtpBase<SafeAsyncScope<CancelViaParent>> {
 protected:
  template <typename>
  friend class detail::SafeAsyncScopeCrtpBase;

  auto scopeCancellationTuple() const { return std::tuple{}; }
};

/// Helper to construct `SafeAsyncScope` args.
///
/// Future: If needed, we could extend this to support passing a
/// cancellation token to `safeAsyncScope()`, to be automatically merged
/// with any other cancellation tokens specified by the scope's policy.
template <typename CancelPolicy>
auto safeAsyncScope() {
  return as_capture(
      ::folly::bindings::make_in_place<SafeAsyncScope<CancelPolicy>>());
}

template <safe_alias, typename>
class SafeTask;

namespace detail {

template <typename T, safe_alias MinSafety>
concept is_void_safe_task = safe_task_traits<T>::arg_safety >= MinSafety &&
    std::is_void_v<typename safe_task_traits<T>::return_type>;

template <typename>
inline constexpr bool is_SafeAsyncScopeContextProxy_v = false;
template <typename CancelPolicy, size_t NumTokens>
inline constexpr bool is_SafeAsyncScopeContextProxy_v<
    SafeAsyncScopeContextProxy<CancelPolicy, NumTokens>> = true;
template <typename T>
concept is_SafeAsyncScopeContextProxy = is_SafeAsyncScopeContextProxy_v<T>;

} // namespace detail

template <typename CancelPolicy, size_t NumTokens>
class SafeAsyncScopeContextProxy : NonCopyableNonMovable {
 public:
  // This takes only `ValueTask<void>` or `CoCleanupSafeTask<void>`.  If you
  // need the callback to schedule more callbacks on the same arg, use
  // `scheduleScopeClosure()`, and use scope ref it gives as the first arg.
  void schedule(
      detail::is_void_safe_task<safe_alias::co_cleanup_safe_ref> auto t) && {
    s_.scope_.add(
        std::move(t).scheduleOnSafe(std::move(s_.executor_)),
        std::move(s_.tokensToMerge_),
        std::move(s_.sourceLoc_),
        s_.returnAddress_,
        [](const CancellationToken&) {});
  }

  // Pass a safe ref to the current scope as the closure's first arg, as if:
  //   schedule(async_closure(args[0], thisScope, args[1:]...))
  //
  // Rationale: Giving a closure a ref to a scope (or any other `co_cleanup`
  // object) would lower its safety to the point where it can't be scheduled
  // on a scope.  The safety downgrade is necessary, since cleanup order is
  // unspecified, so the thing you're referencing may have been cleaned up.
  // In contrast, the reference to the **current** scope is always safe.
  //
  // P.S.  Making `schedule(async_closure(..., scope))` work would be great,
  // but isn't very feasible, since to do this safely we'd need to introduce
  // a new `SafeTask` category that distinguishes the "current async scope"
  // args (which resolves to `co_cleanup_capture<Scope&>` for the inner
  // closure) from the safety of other args.
  auto scheduleScopeClosure(auto make_inner_coro, auto&&... args) && {
    // It's only safe to pass `co_cleanup_capture<Scope&>` into the
    // sub-closure because `AsyncScope::join` guarantees that the scope will
    // remain valid as long as the sub-closure is alive.
    //
    // The sub-closure's own `captures` will still be downgraded to
    // `after_cleanup_ref`, even though the sub-closure will NOT be created as
    // `shared_cleanup` (a hack to satisfy the `schedule()` safety
    // constraints).
    //
    // Future: Even if `this` sat in `restricted_co_cleanup_capture`, there's
    // a free choice about whether to emit a restricted or unrestricted ref
    // -- this just affects the sub-closure.  We can later add
    // `scheduleRestricedScopeClosure` for that.
    //
    // Future: It is defensible to make this `by_non_const_ref`, but for the
    // moment, the only operation of interest is `add()`, which is `const`.
    // Starting cleanup is non-`const`, but even if we had a public API,
    // awaiting that cleanup from a sub-closure would cause a deadlock,
    // hence, dubious.
    auto [b] = folly::bindings::by_ref(s_.scope_);
    return std::move(*this).schedule(async_closure(
        std::move(make_inner_coro),
        detail::async_closure_scope_self_ref_hack<
            co_cleanup_capture<typename decltype(b)::binding_type>,
            decltype(b)>{std::move(b)},
        std::forward<decltype(args)>(args)...));
  }

  // PRIVATE, for `asyncObjectPtr` only.  This isn't ready to go public
  // since per `WithParentCancelToken`, we may make it return a
  // pointer-to-token.  Also, a public version of this API ought to signal
  // whether the task already had a token.
  CancellationToken scheduleReturningCancelToken(
      async_closure_private_t, // `AsyncObjectTag` is incomplete here
      detail::is_void_safe_task<safe_alias::co_cleanup_safe_ref> auto t) && {
    CancellationToken ctok;
    s_.scope_.add(
        std::move(t).scheduleOnSafe(std::move(s_.executor_)),
        std::move(s_.tokensToMerge_),
        std::move(s_.sourceLoc_),
        s_.returnAddress_,
        [&](const CancellationToken& ct) { ctok = ct; });
    return ctok;
  }

 private:
  template <typename>
  friend class SafeAsyncScopeProxy;

  struct State {
    SafeAsyncScope<CancelPolicy>& scope_;
    Executor::KeepAlive<> executor_;
    std::array<CancellationToken, NumTokens> tokensToMerge_;
    source_location sourceLoc_;
    void* returnAddress_;
  };

  explicit SafeAsyncScopeContextProxy(State s) : s_(std::move(s)) {}

  State s_;
};

// Notes on `add()` and `with().schedule()`
//
// (1) WARNING: If your awaitable has a pre-existing cancellation token (had
// `co_withCancellation` called on it), then none of the cancellation tokens
// managed by `SafeAsyncScope` will apply.  So, instead of doing that, pass
// your cancellation tokens via `tokensToMerge`.
//
// Your per-awaitable `tokensToMerge` will be merged with any scope- or
// closure-level tokens configured by the scope.  Empty cancellation
// tokens are very cheap, so don't worry about the overhead.
//
// (2) `add()` and `with()` (for `.schedule()`) are marked `const` because
// concurrent` calls are thread-safe with respect to one another (but not
// with respect to `join` / `cleanup`), see `SafeAsyncScopeCrtpBase:add()`.
//
// This is neither copyable nor movable because users are only supposed to
// access these methods via `co_cleanup_capture<SafeAsyncScope<...>>`, which
// temporarily instantiates this proxy object via `capture_*_proxy`.
template <typename CancelPolicy>
class SafeAsyncScopeProxy : NonCopyableNonMovable {
 public:
  // This is a workaround for combining default arguments (for
  // debuggability) with a perfect-forwarded parameter pack, e.g.:
  //   scope->with(co_await co_current_executor).scheduleScopeClosure(...)
  // `WithContext` is a separate object so that it can be plumbed into other
  // code, like `asyncObjectPtr`.  Using a binding tuple for the pack would
  // also work, but the API would be less explicit and less modular.
  //
  // NB: `FOLLY_NOINLINE` is required by `FOLLY_ASYNC_STACK_RETURN_ADDRESS`.
  template <size_t NumTokens = 0>
  FOLLY_NOINLINE auto with(
      Executor::KeepAlive<> executor,
      std::array<CancellationToken, NumTokens> tokensToMerge = {},
      source_location sourceLoc = source_location::current(),
      void* returnAddress = FOLLY_ASYNC_STACK_RETURN_ADDRESS()) const {
    using WithCtx = SafeAsyncScopeContextProxy<CancelPolicy, NumTokens>;
    return WithCtx{typename WithCtx::State{
        .scope_ = scope_,
        .executor_ = std::move(executor),
        .tokensToMerge_ = std::move(tokensToMerge),
        .sourceLoc_ = std::move(sourceLoc),
        .returnAddress_ = returnAddress}};
  }

  // NB: `FOLLY_NOINLINE` is required by `FOLLY_ASYNC_STACK_RETURN_ADDRESS`.
  template <size_t NumTokens = 0>
  FOLLY_NOINLINE void add(
      detail::is_void_safe_task_with_executor<
          safe_alias::co_cleanup_safe_ref> auto t,
      std::array<CancellationToken, NumTokens> tokensToMerge = {},
      source_location sourceLoc = source_location::current(),
      void* returnAddress = FOLLY_ASYNC_STACK_RETURN_ADDRESS()) const {
    scope_.add(
        std::move(t),
        std::move(tokensToMerge),
        std::move(sourceLoc),
        returnAddress,
        [](const CancellationToken&) {});
  }

  void requestCancellation() const noexcept
    requires requires(SafeAsyncScope<CancelPolicy> s) {
      s.requestCancellation();
    }
  {
    scope_.requestCancellation();
  }

  // Only used to resolve `->` for `co_cleanup_capture<SafeAsyncScope<...>>`
  // via `capture_ptr_proxy`.
  const SafeAsyncScopeProxy* operator->() const {
    return static_cast<const SafeAsyncScopeProxy*>(this);
  }

 protected:
  template <typename>
  friend class detail::SafeAsyncScopeCrtpBase;

  SafeAsyncScopeProxy(SafeAsyncScope<CancelPolicy>& s) : scope_(s) {}

 private:
  SafeAsyncScope<CancelPolicy>& scope_;
};
template <typename CancelPolicy>
SafeAsyncScopeProxy(SafeAsyncScope<CancelPolicy>)
    -> SafeAsyncScopeProxy<CancelPolicy>;

} // namespace folly::coro

#endif
