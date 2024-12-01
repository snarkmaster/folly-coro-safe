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

#include <folly/coro/safe/SafeTask.h>
#include <folly/coro/safe/detail/Bindings.h>

#ifndef _WIN32 // Explained in SafeTask.h

/* XXX
Read `SaferCoro.md` for an intro. Key points to know when touching this header:
  - `asyncClosure()` wraps its inner awaitable with another scope.  For
    special argument types, this:
      * Ensures that these args outlive the inner task & subsequent cleanup,
        making them usable in tasks scheduled on `SafeAsyncScope`s.
      * Runs exception-safe async cleanup for `SafeAsyncScope` and similar.
      * Allows passing `AsyncClosure*Ptr` and `AsyncClosureNamed`
        by-reference to descendant `SafeTask`s.  These references cannot be
        **returned** from `SafeTask` since it'd be easy to accidentally
        return them out of the owning scope.
*/

namespace folly::coro::detail {

void async_closure_set_cancel_token(
    async_closure_private_t priv, auto&& arg, const CancellationToken& ctok) {
  if constexpr ( // DO NOT USE: for AsyncObject only
      requires { arg.privateHackSetParentCancelToken(arg, priv, ctok); }) {
    arg.privateHackSetParentCancelToken(arg, priv, ctok);
  } else if constexpr ( //
      requires {
        {
          arg.get_lref().setParentCancelToken(priv, ctok)
        } -> std::same_as<void>;
      }) {
    arg.get_lref().setParentCancelToken(priv, ctok);
  }
}

auto async_closure_make_cleanup_tuple(
    async_closure_private_t priv, auto&& arg, const exception_wrapper* err) {
  // `co_cleanup` is allowed to return `Task<void>` or a tuple of them.
  auto task_or_tup = [&]() {
    if constexpr (has_async_object_private_hack_co_cleanup<decltype(arg)>) {
      return arg.privateHack_co_cleanup(std::move(arg), priv, err);
    } else {
      using ArgT =
          typename std::remove_reference_t<decltype(arg)>::capture_type;
      if constexpr (has_async_closure_co_cleanup_with_error<ArgT>) {
        return std::move(arg.get_lref()).co_cleanup(priv, err);
      } else if constexpr (has_async_closure_co_cleanup_error_oblivious<ArgT>) {
        return std::move(arg.get_lref()).co_cleanup(priv);
      } else {
        return std::tuple{};
      }
    }
  }();
  if constexpr (std::is_same_v<decltype(task_or_tup), Task<void>>) {
    return std::tuple{std::move(task_or_tup)};
  } else {
    return std::apply(
        [&](auto... cs) { return std::tuple{std::move(cs)...}; },
        std::move(task_or_tup));
  }
}

template <typename T>
concept has_result_after_cleanup =
    requires(lift_unit_t<T> t, async_closure_private_t priv) {
      std::move(t).result_after_cleanup(priv);
    };

auto async_closure_outer_coro_result(async_closure_private_t priv, auto r) {
  if constexpr (has_result_after_cleanup<
                    std::remove_reference_t<decltype(r)>>) {
    return std::move(r).result_after_cleanup(priv);
  } else {
    (void)priv;
    return r;
  }
}

template <bool SetCancelTok, typename ResultT, safe_alias OuterSafety>
auto async_closure_make_outer_coro(
    async_closure_private_t priv, auto inner_coro, auto storage_ptr) {
  return std::apply(
      [&](auto... cleanup_try_tasks) {
        // Hack alert: This makes a `SafeTask` right away just because
        // there's currently no `co_awaitTry` introspection, but...
        // technically it should be `unsafe`, and when that happens, we'd
        // need to use another hack like `withNewSafety` here.
        return async_closure_outer_coro<SetCancelTok, ResultT, OuterSafety>(
            priv,
            co_awaitTry(std::move(inner_coro)),
            std::move(storage_ptr),
            co_awaitTry(std::move(cleanup_try_tasks))...);
      },
      // NO CONTRACT on cleanup order.  The construction order of the
      // "storage" tuple is unspecified, so we can't promise a reasonable
      // thing like "reverse of construction order".  While it'd be easy
      // enough to use an ordered-construction tuple, the more durable
      // argument is that we may later want `collectAll()` for the cleanup.
      //
      // `bad_alloc` safety: make the cleanup tasks before awaiting the
      // inner coro.  The `inner_err_` doc explains why this sits outside
      // the outer coro -- in short, this saves us a coro frame allocation.
      std::apply( // Merge `co_cleanup` tuples from all the args
          [&](auto&... args) {
            return std::tuple_cat(async_closure_make_cleanup_tuple(
                priv, args, storage_ptr->inner_err_ptr())...);
          },
          storage_ptr->storage_tuple_like()));
}

template <
    bool SetCancelTok,
    typename ResultT,
    safe_alias OuterSafety,
    typename OuterResT = drop_unit_t<decltype(async_closure_outer_coro_result(
        std::declval<async_closure_private_t>(),
        std::declval<lift_unit_t<ResultT>&&>()))>>
SafeTask<OuterSafety, OuterResT> async_closure_outer_coro(
    async_closure_private_t priv,
    auto try_inner,
    auto storage_ptr,
    auto... cleanup_try_tasks) {
  auto& inner_err = *storage_ptr->inner_err_ptr();
  if constexpr (kIsDebug) {
    inner_err.reset(); // Clear `BUG_co_cleanup_must_not_copy_error`
  }

  // Pass our cancellation token to args that want it for cleanup.  The user
  // code can throw -- e.g. `CancellationToken::merge()` may allocate.
  if constexpr (SetCancelTok) {
    const auto& ctok = co_await co_current_cancellation_token;
    inner_err = try_and_catch([&]() {
      std::apply(
          [&](auto&&... args) {
            (async_closure_set_cancel_token(priv, args, ctok), ...);
          },
          storage_ptr->storage_tuple_like());
    });
  }

  // Await the inner task (unless some `setParentCancelToken` failed)
  Try<ResultT> res;
  if (!inner_err) {
    // NOTE: Here and below, assume that the semi-awaitable `co_viaIfAsync`
    // machinery for `Task` (or whatever `try_inner`) is non-throwing.
    // I would love a `static_assert(noexcept(...))` to prove this, but that
    // requires plumbing `noexcept(noexcept(...))` annotations through more
    // of `ViaIfAsync.h`.
    res = co_await std::move(try_inner);
    if (res.hasException()) {
      inner_err = std::move(res.exception());
    }
  }

  folly::exception_wrapper cleanup_err;
  auto recordError = [&cleanup_err](folly::Try<void> clean_res) {
    // NOT A CONTRACT, but for now, the first exception _might_ be the
    // most informative?  In the future, we could provide e.g. a
    // collated exception object of up to K most recent exceptions,
    // perhaps with K different for opt and debug builds.
    if (!cleanup_err && clean_res.hasException()) {
      cleanup_err = std::move(clean_res).exception();
    }
  };
  // A comma-fold lets us await all the cleanup tasks in the current coro frame
  (recordError(co_await std::move(cleanup_try_tasks)), ...);
  if (cleanup_err) {
    co_yield folly::coro::co_error{std::move(cleanup_err)};
  }

  if (LIKELY(res.hasValue())) {
    if constexpr (std::is_void_v<ResultT>) {
      co_return;
    } else {
      co_return async_closure_outer_coro_result(priv, std::move(res).value());
    }
  } else if (LIKELY(res.hasException())) {
    co_yield co_error(std::move(inner_err));
  } else { // should never happen
    co_yield co_error(UsingUninitializedTry{});
  }
  (void)storage_ptr; // This param keeps the stored args alive
}

// E.g. maps <0, 2, 1, 0, 2> to <0, 2, 3, 3>
template <auto Sum, auto...>
inline constexpr auto cumsum_except_last = vtag<>;
template <auto Sum, auto Head, auto... Tail>
inline constexpr auto cumsum_except_last<Sum, Head, Tail...> =
    []<auto... Vs>(vtag_t<Vs...>) {
      return vtag<Sum, Vs...>;
    }(cumsum_except_last<Sum + Head, Tail...>);

static_assert(std::is_same_v< // ХХХ to test
              decltype(cumsum_except_last<0, 2, 1, 3>),
              const vtag_t<0, 2, 3>>);

// When returned from `bind_captures_to_closure`, this wraps a coroutine
// instance.  This reconciles two goals:
//  - Let tests cover the `is_safe()` logic.
//  - `static_assert()` the closure's safety before releasing it.
//
// Closure safety checks follow the model of `SafeTask.h` -- and actually
// reuse most of that implementation by requiring the inner coro to be a
// `SafeTask`.
//
// Note that we don't check whether the callable passed into
// `async_closure()` is stateless, and we don't need to -- it is executed
// eagerly, and may be a coroutine wrapper.  The coro callable underlying
// the inner `SafeTask` will have been verified to be stateless.
//
// Future: An `AsyncGenerator` closure flavor is possible, just think about
// safety assertions on the yielded type, and review
// https://fburl.com/asyncgenerator_delegation
template < // inner coro safety is measured BEFORE re-wrapping it!
    safe_alias OuterSafety,
    safe_alias InnerSafety,
    typename MakeOuterCoro>
class async_closure_wrap_coro {
 private:
  MakeOuterCoro make_outer_coro_;

 protected:
  template <bool>
  friend auto bind_captures_to_closure(auto, auto&&...);
  explicit async_closure_wrap_coro(
      vtag_t<OuterSafety, InnerSafety>, MakeOuterCoro make_outer)
      : make_outer_coro_(std::move(make_outer)) {}

 public:
  // Don't allow closures with `unsafe*` args.
  static constexpr bool has_safe_args =
      (OuterSafety >= safe_alias::closure_min_arg_safety);

  // The reason we need `SafeTask` here is that it have already detected any
  // by-reference arguments (impossible to detect otherwise), stateful
  // coros, and unsafe return types.
  static constexpr bool is_inner_coro_safe =
      (InnerSafety >= safe_alias::unsafe_closure_internal);

  // KEEP IN SYNC with `release_outer_coro`. Separate for testing.
  static consteval bool is_safe() {
    return has_safe_args && is_inner_coro_safe;
  }

  // Delay the `static_assert`s so we can test `bind_captures_to_closure`
  // on unsafe inputs.
  auto release_outer_coro() && {
    // KEEP IN SYNC with `is_safe`.
    static_assert(
        has_safe_args,
        "Args passed into `async_closure` must have `safe_alias_of_v` of at "
        "least `shared_cleanup`. `NowTask` does not have this constraint. If "
        "you naeed a closure, use `manual_safe_*` to work around this, and "
        "comment with a proof of why your usage is memory-safe.");
    static_assert(
        is_inner_coro_safe,
        "`async_closure` currently only supports `SafeTask` as the inner coro.");
    return make_outer_coro_();
  }
};

// The compiler cannot deduce that `async_closure_outer_stored_arg` cannot
// occur when `storage_ptr` is `nullopt_t`.  This helper function just
// delays instantiation of `storage_ptr->`.
template <size_t Idx>
decltype(auto) get_from_storage_ptr(auto& p) {
  return std::get<Idx>(p->storage_tuple_like());
}

auto async_closure_default_inner_err() {
  if constexpr (kIsDebug) {
    // If you see this diagnostic, check that your
    // `co_cleanup` does not inadvertently copy the
    // `exception_wrapper` parameter before creating the coro
    // frame.  Store the provided pointer instead.
    struct BUG_co_cleanup_must_not_copy_error : std::exception {};
    return make_exception_wrapper<BUG_co_cleanup_must_not_copy_error>();
  } else {
    return exception_wrapper{};
  }
}

template <typename... Ts>
struct async_closure_storage {
  explicit async_closure_storage(auto&&... as)
      : inner_err_(async_closure_default_inner_err()),
        storage_tuple_(std::forward<decltype(as)>(as)...) {}

  // We go through getters so that `AsyncObject` can reuse closure machinery.
  // Note that we only need lvalue refs to the storage tuple, meaning that
  // returning a ref-to-a-tuple is as good as a tuple-of-refs here.
  // We return an rvalue ref for compatibility with the latter scenario.
  auto&& storage_tuple_like() { return storage_tuple_; }
  auto* inner_err_ptr() { return &inner_err_; }

  // For `bad_alloc` safety, we must create the cleanup coros before
  // awaiting the inner coro.  This preallocated exception (which is passed
  // to the cleanup coros by-reference) further enables us to create the
  // cleanup coros before we even create the outer coro.  That avoids an
  // extra coro frame that would otherwise be neeed to await a cleanup tuple.
  exception_wrapper inner_err_;
  std::tuple<Ts...> storage_tuple_;
};

template <size_t StorageI, typename Bs>
decltype(auto) async_closure_bind_inner_coro_arg(
    capture_private_t priv, Bs& bs, auto& storage_ptr) {
  if constexpr (is_instantiation_of_v<async_closure_outer_stored_arg, Bs>) {
    // "own": arg was already moved into `storage_ptr`.
    auto& storage_ref = get_from_storage_ptr<StorageI>(storage_ptr);
    static_assert(std::is_same_v<
                  typename Bs::storage_type,
                  std::remove_reference_t<decltype(storage_ref)>>);
    // `SharedCleanupClosure=true` preserves the `after_cleanup_ref_` prefix of
    // the storage type.
    return storage_ref.template to_capture_ref</*shared*/ true>(priv);
  } else if constexpr (
      is_instantiation_of_v<async_closure_inner_stored_arg, Bs> ||
      is_instantiation_of_v<async_closure_scope_self_ref_hack, Bs>) {
    // "own": Move stored `as_capture()` into inner coro.
    return typename Bs::storage_type{priv, std::move(bs.binding)};
  } else if constexpr (is_any_capture<Bs>) {
    // "pass": Move `capture<Ref>` into the inner coro.
    static_assert(std::is_reference_v<typename Bs::capture_type>);
    return std::move(bs);
  } else { // "regular": Non-`capture` binding.
    return std::move(bs).what_to_bind();
  }
}

// Eagerly construct -- but do not await -- an `async_closure()`:
//   - Resolve bindings.
//   - Construct & store args for the user-supplied inner coro.
//   - For ensuring cleanup in the face of `bad_alloc`, pre-allocate the
//     outer task & `co_cleanup` tasks, if needed.
//   - Create the inner coro, passing it `capture` references, or -- if
//     there are no `co_cleanup` args and no outer coro -- quack-alike
//     owning wrappers.
//   - Marks the final user-facing task with the `safe_alias` that
//     describes the memory-safety of the closure's arguments.
//   - Returns the task inside a wrapper that statically checks the memory
//     safety of the return & `make_inner_coro` types when
//     `release_outer_coro()` is called.
//
// NB: Due to the "omit outer coro" optimization, `release_outer_coro()`
// will in some cases return a no-overhead wrapper around the coro returned
// by `make_inner_coro()`.
//
// Rationale: "Eager" is the only option matching user expectations, since
// regular coroutine args are bound eagerly too.  Implementation-wise, all
// `lang/Bindings.h` logic has to be resolved within the current statement,
// since the auxiliary reference-bearing objects aren't valid beyond that.
template <bool ForceOuterCoro>
auto bind_captures_to_closure(auto make_inner_coro, auto&&... args) {
  auto [non_storage_safeties, b_tup] =
      transform_capture_bindings<ForceOuterCoro>(
          std::forward<decltype(args)>(args)...);

  // If some arguments require outer-coro storage, construct them in-place
  // on a `unique_ptr<tuple<>>`.  Without an outer coro, this stores `nullopt`.
  //
  // Rationale: Storing on-heap allows the outer coro own the arguments,
  // while simultaneously providing stable pointers to be passed into the
  // inner coro.
  //
  // Future: With a custom coro class, it should be possible to store the
  // argument tuple ON the coro frame, saving one allocation.
  auto storage_ptr = std::apply(
      []<typename... SAs>(SAs... sas) {
        if constexpr (sizeof...(SAs) == 0) {
          return std::nullopt; // Signals "no outer closure" to the caller
        } else {
          // (2) Construct all the storage args in-place in one tuple.
          return std::make_unique<
              async_closure_storage<typename SAs::storage_type...>>(
              folly::bindings::make_in_place<
                  typename decltype(sas)::storage_type>(
                  capture_private_t{}, std::move(sas.binding))
                  .what_to_bind()...);
        }
      },
      // (1) Collect the args that need storage on the outer coro.
      std::apply(
          [](auto&... bs) {
            return std::tuple_cat([]<typename B>(B& b) {
              if constexpr ( //
                  is_instantiation_of_v<async_closure_outer_stored_arg, B>) {
                return std::tuple{std::move(b)};
              } else {
                return std::tuple{};
              }
            }(bs)...);
          },
          b_tup));

  constexpr bool isInvokeMember = is_instantiation_of_v<
      invoke_member_wrapper_fn,
      decltype(make_inner_coro)>;
  auto inner_coro = std::apply(
      [&]<typename... Bs>(Bs&... bs) {
        return [&]<size_t... ArgIs, size_t... StorageIs>(
                   std::index_sequence<ArgIs...>, vtag_t<StorageIs...>) {
          return make_inner_coro(
              // Unpack `Bs`, `ArgIs`, and `StorageIs` jointly
              [&]() -> decltype(auto) {
                if constexpr (isInvokeMember && ArgIs == 0) {
                  // We have a `FOLLY_INVOKE_MEMBER`.  It accesses the
                  // member function via `.`, but this arg is expected to be
                  // `co_cleanup_capture<>` or `AsyncObjectPtr<>`, so we
                  // "magically" dereference it here.
                  //
                  // On safety: Below, we assert that it it made a
                  // `MemberTask<T>`, which `inner_rewrapped` will
                  // implicitly unwrap & mark with a higher safety level.
                  // `MemberTask` provides only a minimal safety
                  // attestation, namely (besides arg 1, the implicit object
                  // param), none of its args are taken by-reference.  This
                  // is fine, since for `OuterSafety`, we will have
                  // accounted for all the args' safety levels.
                  return *async_closure_bind_inner_coro_arg<StorageIs, Bs>(
                      capture_private_t{}, bs, storage_ptr);
                } else {
                  return async_closure_bind_inner_coro_arg<StorageIs, Bs>(
                      capture_private_t{}, bs, storage_ptr);
                }
              }()...);
        }(std::index_sequence_for<Bs...>{},
               cumsum_except_last< // `StorageIs` indexes into `storage_ptr`
                   (size_t)0,
                   is_instantiation_of_v<
                       async_closure_outer_stored_arg,
                       Bs>...>);
      },
      b_tup);

  constexpr safe_alias OuterSafety =
      folly::detail::least_safe_alias(decltype(non_storage_safeties){});
  constexpr safe_alias InnerSafety =
      safe_task_traits<decltype(inner_coro)>::arg_safety;
  // `ClosureTask` & `MemberTask` are non-movable, so we must unwrap them
  // before moving them into the `make_outer_coro` lambda below.
  auto inner_rewrapped = [&] {
    if constexpr (InnerSafety >= safe_alias::unsafe_closure_internal) {
      // We clip to `min_arg_safety` only to enable a `checkIsUnsafe`
      // unit-test, and to improve the compile error when the inner coro is
      // a `SafeTask`, but one of the args being passed is unsafe.  This
      // rewrapped task won't actually be used.
      constexpr auto newSafety =
          std::max(OuterSafety, safe_alias::closure_min_arg_safety);
      // In the presence of stored `capture`s, the out-of-the-box inner
      // coro `safe_alias_of_v` is not going to be what we want, because
      // `Captures.h` marks owned captures as `unsafe_closure_internal` to
      // discourage them being moved out of the closure.  Instead, we set
      // safety based only on the non-storage arguments.
      return std::move(inner_coro).template withNewSafety<newSafety>();
    } else {
      // This is a `SafeTask`, `release_outer_coro()` will fail.
      // Tests covering this: `checkIsUnsafe` and `nonSafeTaskIsNotAwaited`
      return std::move(inner_coro);
    }
  }();
  return async_closure_wrap_coro{
      vtag<OuterSafety, InnerSafety>,
      [tup_ptr = std::move(storage_ptr),
       inner = std::move(inner_rewrapped)]() mutable {
        using ResultT = semi_await_result_t<decltype(inner)>;
        // We require this calling convention because the `isInvokeMember`
        // branch above dereferences the 1st arg.  That is only sensible if
        // we KNOW that the arg is the implicit object parameter, which
        // would not be true e.g.  if the user passed something like this:
        //   [](int num, auto me) { return me->addNumber(num); }
        static_assert(
            std::is_same_v<MemberTask<ResultT>, decltype(inner_coro)> ==
                isInvokeMember,
            "To use `MemberTask<>` coros with `async_closure()`, you must pass "
            "callable as `FOLLY_INVOKE_MEMBER(memberName)`, and pass the "
            "instance's `capture`/`AsyncObjectPtr`/... as the first argument.");
        if constexpr (std::is_same_v<decltype(tup_ptr), std::nullopt_t>) {
          // No outer coro is needed, so we can return the inner one.
          static_assert(
              !has_result_after_cleanup<ResultT>,
              "Cannot `co_return *after_cleanup()` without a cleanup arg");
          return std::move(inner);
          (void)tup_ptr;
        } else {
          return async_closure_make_outer_coro<
              /*cancelTok*/ true,
              ResultT,
              OuterSafety>(
              async_closure_private_t{}, std::move(inner), std::move(tup_ptr));
        }
      }};
}

} // namespace folly::coro::detail

#endif
