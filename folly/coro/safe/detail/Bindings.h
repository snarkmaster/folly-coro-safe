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

#include <compare>

#include <folly/coro/safe/Captures.h>

namespace folly::coro {
class AsyncObject;
class AsyncScopeSlotObject;
template <typename, size_t>
class SafeAsyncScopeContextProxy;
} // namespace folly::coro

namespace folly::coro::detail {

// These tag classes wrap closure-owned `as_capture()` bindings.  They're
// separate to simplify branching in the business logic.  For "outer coro"
// args, we instantiate the bindings in-place on the `unique_ptr<tuple<>>`.
// Without an outer coro, we have to instead move the instantiated args into
// the inner coro.  In both cases, we delay instantiation until the last
// moment to minimize copies, and to correctly support in-place,
// non-movable, non-copyable data storage on the outer coro.
//
// For all other cases, `transform_binding()` just returns an unwrapped
// final binding.  In `capture_binding_helper` jargon, these cases are:
//   - "pass" `*capture*`s from the caller,
//   - "regular" args bound without `*capture*` tooling.
template <is_any_capture Storage, typename Binding>
struct async_closure_outer_stored_arg {
  using storage_type = Storage;
  Binding binding;
};
template <is_any_capture Storage, typename Binding>
struct async_closure_inner_stored_arg {
  using storage_type = Storage;
  Binding binding;
};

// Hack: This is used by `schedule*Closure()` to give its sub-closure a
// `shared_cleanup` reference that is KNOWN to be safe within that
// sub-closure only (the subclosure keeps the reference object alive, since
// `AsyncScope::join` blocks until ALL awaitables finish).
//
// This is NOT safe to use in other settings, because:
// (1) We instantiates a new `capture` ref from a bare reference.
// (2) This `ref_hack` object is deliberately excluded from the final
//     `async_closure`'s safety accounting -- it's only affects the
//     `shard_cleanup` downgrade.
// Both of these are only OK because the sub-closure's own scope outlives it.
template <typename Storage, typename Binding>
struct async_closure_scope_self_ref_hack {
  using storage_type = Storage;

 protected:
  template <size_t, typename Bs>
  friend decltype(auto) async_closure_bind_inner_coro_arg(
      capture_private_t, Bs&, auto&);
  template <typename, size_t>
  friend class ::folly::coro::SafeAsyncScopeContextProxy;
  friend class ::folly::coro::AsyncScopeSlotObject;
  // The innards are `protected`, since `transform_binding` lets this be
  // supplied from outside the closure machinery.  Any new client being
  // added here must think THOROUGHLY about the risks in the class docblock!
  async_closure_scope_self_ref_hack(Binding b) : binding(std::move(b)) {}
  Binding binding;
};

// To provide memory-safety guarantees, `*capture*` usage in
// `async_closure` has special rules.  Read `SafeAlias.h` and `Captures.h`
// docs first.  These rules extend the regular `binding_policy` from
// `lang/Binding.h`, and apply only when creating or passing `*capture*`s.
//
// `capture_binding_helper` has 3 types of per-argument binding rules:
//   "own": `as_capture()` bindings, for which this closure owns storage.
//       If this new closure does has "shared cleanup", this branch will
//       emit `body_only_capture*<T>` types.  `T` will be a reference
//       for closures with an outer coro, and a value otherwise.
//   "pass": Pass preexisting `capture`s (NOT owned by this closure).  If
//       this new closure has "independent cleanup", this branch will drop
//       the `body_only_ref_` prefix from the `capture` type. Sub-cases:
//         - "pass.makeRef": Transform `capture<Val>` to `capture<Ref>`.
//         - "pass.forwardRef": Pass `capture<Ref>` by-value.
//             * Copy `capture<Ref>` bound as lval ref
//             * Move: For input `capture<Ref>` bound as rval ref, pass
//               the child a new `capture<Val&&>`.
//   "regular": Other arguments use `folly/lang/Bindings.h` semantics.
//
// These value types can be stored on an outer coro:
//   capture<V>, body_only_capture<V>
//   capture_unique<V>, body_only_capture_unique<V>
//   co_cleanup_capture<V>
//   restricted_co_cleanup_capture<V> -- add later via below "Future:" notes
//
// These value types can be moved into an inner coro:
//   capture<V>, body_only_capture<V>
//   capture_unique<V>, body_only_capture_unique<V>
//   capture_heap<V>, body_only_capture_heap<V> -- for `make_in_place*`
//
// These reference types can be passed to the inner coro:
//   capture <T&> or <T&&>, body_only_capture <T&> or <T&&>
//   co_cleanup_capture<T&>
//   restricted_co_cleanup_capture<T&> -- add later via below "Future:" notes
//
// The full correspondence between value & reference types is specified in
// `to_capture_ref()` from `Captures.h`.  In short, the cleanup value
// types map to the eponymous reference types.  Non-cleanup types all map to
// `capture` or `body_only_capture`, following the shared-cleanup
// closure rules.
struct binding_helper_cfg {
  bool is_shared_cleanup_closure;
  bool has_outer_coro;
  constexpr auto operator<=>(const binding_helper_cfg&) const = default;
};
template <typename B, auto Cfg> // `B` is the input binding
class capture_binding_helper {
 private:
  static_assert(!std::is_reference_v<B>);
  // A template parameter constraint would make forward-declarations messy.
  static_assert(std::is_same_v<decltype(Cfg), binding_helper_cfg>);

  using CP = ::folly::bindings::detail::category_projection;
  using BP = ::folly::bindings::detail::binding_policy<B>;
  using ST = typename BP::storage_type;
  using UncvrefST = std::remove_cvref_t<ST>;

  // Validation for the "pass" case.  Here, `ST` is either an `*capture*`
  // or a reference to one.  `RetST` is the child's `*capture*<Ref>`.
  //
  // There is no "Business Logic" here.  This documents the possible data
  // flows,  `static_asserts` a couple of likely user errors for better
  // usability, and provides a guide to the relevant unit tests.
  template <typename RetST>
  static constexpr void static_assert_passing_capture() {
    using ArgT = typename UncvrefST::capture_type;
    static_assert( // Self-test
        std::is_reference_v<ST> == (B::bind_info.category_p == CP::ref));
    if constexpr (std::is_reference_v<ArgT>) { // Is `capture<Ref>`?
      static_assert(
          !std::is_reference_v<ST>,
          "Pass `capture<Ref>` by value, do not use `by_ref`");
      // "pass.forwardRef": Pass `capture<Ref>` by-value.
      if constexpr (std::is_lvalue_reference_v<typename B::binding_type>) {
        // `check_capture_lref_to_lref`: We're storing an `capture<Ref>`
        // by-value, and it's bound to the input by lval ref.  `b` will either
        // copy the `capture<Ref>`, or upgrade `body_only_capture`.

        static_assert( // Improve errors over just "deleted copy ctor".
            !std::is_rvalue_reference_v<ArgT>,
            "capture<V&&> is move-only. Try std::move(yourRef).");
      } else {
        // `check_capture_lref_to_rref`, `check_capture_rref_to_rref`:
        // Moving input `capture<Ref>` bound as rval ref gives the child a
        // an `capture<Val&&>`.
        //
        // It is in some sense optional to support this, since users can
        // pass around lval refs and `std::move(*argRef)` at the last
        // minute.  However, I wanted to encourage the best practice of
        // `std::move(argRef)` at the outermost callsite that knows about
        // the move.  Reasons:
        //   - The initial `std::move(arg)` enables use-after-move linting
        //     in the outermost scope.
        //   - `capture<T&&>` is move-only, meaning subsequent scopes also
        //     get use-after-move linting.
        //   - Future: we could build a debug-only use-after-move runtime
        //     checker by adding some state on `capture`s.

        // Cleanup args don't support rval refs
        static_assert(!is_any_co_cleanup_capture<UncvrefST>);
      }
    } else { // Is `capture<Val>` --
      // `check_capture_val_to_ref`: Cleanup args require an outer coro,
      // so it should never be the case that `*co_cleanup_capture<Val>` is
      // being passed to a coro.
      static_assert(!is_any_co_cleanup_capture<UncvrefST>);

      // "pass.makeRef1": `capture<Val>` implicitly becomes `capture<Ref>`.
      //
      // This implicit conversion exists so that closures that lack an outer
      // coro, and get their `capture`s by value, behave more like the
      // outer-coro ones.  I.e. this tries to mask the "no outer coro"
      // optimization a bit better.
      //
      // "pass.makeRef2": `by_ref(capture<Val>)` becomes `capture<Ref>`.
      //
      // We don't require `ST` to be a value on this branch.  This optional
      // choice allows "makeRef2" to work, even though the normal thing is
      // to rely on the implicit "makeRef1" conversion.  The motivation:
      //   - It makes it easier to toggle closure arguments back and forth
      //     between `capture`s and plain by-value arguments.
      //   - There's little downside to this flexibility.
    }
  }

  template <typename T>
  static constexpr auto store_as(auto b) {
    if constexpr (Cfg.has_outer_coro) {
      // The `asyncObjectAsArg` rewrite below alters `decltype(b)` from `B`.
      return async_closure_outer_stored_arg<T, decltype(b)>{
          .binding = std::move(b)};
    } else {
      return async_closure_inner_stored_arg<T, B>{.binding = std::move(b)};
    }
  }

  // "own": The closure must create storage for these `as_capture()` bindings
  static constexpr auto store_capture_binding(B b) {
    static_assert(
        (b.bind_info.category_p != CP::ref) && !std::is_reference_v<ST>,
        "`capture*()` can only store values (not refs) on the closure");
    static_assert(!is_any_capture<ST>);
    if constexpr (has_async_object_private_hack_co_cleanup<ST>) {
      static_assert(
          std::derived_from<ST, AsyncObject>,
          "`privateHack` APIs are private to `AsyncObject`, do not use");
      // Add `AsyncObjectTag` to the partial binding from `asyncObjectAsArg()`
      return std::apply(
          [](auto&& make_tag, auto&&... args) {
            auto [new_b] = as_capture(folly::bindings::make_in_place<ST>(
                make_tag(capture_private_t{}),
                std::forward<decltype(args)>(args)...));
            return store_as<co_cleanup_capture<ST>>(std::move(new_b));
          },
          std::move(b).what_to_bind().release_arg_tuple());
    } else if constexpr (has_async_closure_co_cleanup<ST>) {
      static_assert(Cfg.has_outer_coro);
      // Future: Add a toggle to emit `restricted_co_cleanup_capture`
      return store_as<co_cleanup_capture<ST>>(std::move(b));
    } else if constexpr (B::bind_info.capture_p == capture_projection::unique) {
      if constexpr (Cfg.is_shared_cleanup_closure) {
        return store_as<body_only_capture_unique<ST>>(std::move(b));
      } else {
        return store_as<capture_unique<ST>>(std::move(b));
      }
    } else if constexpr (
        !Cfg.has_outer_coro &&
        folly::bindings::detail::is_in_place_binding_v<B>) {
      if constexpr (Cfg.is_shared_cleanup_closure) {
        return store_as<body_only_capture_heap<ST>>(std::move(b));
      } else {
        return store_as<capture_heap<ST>>(std::move(b));
      }
    } else {
      if constexpr (Cfg.is_shared_cleanup_closure) {
        return store_as<body_only_capture<ST>>(std::move(b));
      } else {
        return store_as<capture<ST>>(std::move(b));
      }
    }
  }

 public:
  // Transforms `b` as described in the class doc, returns a new binding.
  static constexpr auto transform_binding(B b) {
    if constexpr (is_capture_binding<B>) { // `check_stored_*`
      return store_capture_binding(std::move(b));
    } else { // Bindings for arguments the closure does NOT store.
      // Without `as_capture`, the argument will require a copy or a move
      // to be passed to the inner task (which the type may not support).
      // If `as_capture` isn't appropraite, the user can also work around
      // the issue via `std::make_unique<TheirType>`.
      static_assert(
          !folly::bindings::detail::is_in_place_binding_v<B>,
          "Did you mean `as_capture(make_in_place<T>(...))`?");
      if constexpr (is_any_capture<UncvrefST>) { // `check_capture_*`
        // "pass": Pass preexisting `capture`s (NOT owned by this closure).
        // Future: Add a toggle to make `restricted_co_cleanup_capture` refs.
        auto arg_ref =
            std::move(b)
                .what_to_bind()
                .template to_capture_ref<Cfg.is_shared_cleanup_closure>(
                    capture_private_t{});
        static_assert_passing_capture<decltype(arg_ref)>();
        return std::move(arg_ref);
      } else if constexpr ( //
          is_instantiation_of_v<async_closure_scope_self_ref_hack, UncvrefST>) {
        // This `ref_hack` type quacks like the `stored_arg` types, so we
        // need to unwrap it for it to be handly correctly downstream.
        return std::move(b).what_to_bind();
      } else { // `check_regular_args`
        // "regular": Neither a newly stored `as_capture()` nor a parent's
        // `*capture*<T>`.  Follows normal `folly/lang/Bindings.h` rules.

        // Users should not accidentally drop memory-safety protections by
        // passing cleanup args via `*argRef`.  If you must do this for some
        // bizarre reason, please hide your cleanup argument in a wrapper
        // struct, with an explanatory comment.
        static_assert(
            !has_async_closure_co_cleanup<UncvrefST>,
            "This argument implements `async_closure` cleanup, so you should "
            "almost certainly pass it `as_capture()` -- or, if you already "
            "have as a reference `*capture*`, by-value.");

        return std::move(b);
      }
    }
  }
};

// Returns a vtag of `safe_alias_v` for the storage type of the args that
// did not come from `store_capture_binding`.
//
// We have to special-case the stored ones because `Captures.h` makes the
// wrappers for on-closure stored values `unsafe` anyway to discourage users
// from moving them from the original closure.  And, the wrappers themselves
// check the safety of the underlying type (via `capture_safety`).
template <bool IncludeRefHack, auto Cfg, typename TransformedBindingList>
constexpr auto vtag_safety_of_non_stored_args() {
  return []<typename... T>(tag_t<T...>) {
    return value_list_concat_t<
        vtag_t,
        decltype([]() {
          // "own": `store_as` outputs `async_closure_*_stored_arg`.
          if constexpr (
              is_instantiation_of_v<async_closure_outer_stored_arg, T> ||
              is_instantiation_of_v<async_closure_inner_stored_arg, T>) {
            static_assert(
                !std::is_reference_v<typename T::storage_type::capture_type>);
            return vtag<>;
          } else if constexpr ( //
              is_instantiation_of_v<async_closure_scope_self_ref_hack, T>) {
            if constexpr (IncludeRefHack) {
              return vtag<safe_alias_of_v<typename T::storage_type>>;
            } else {
              return vtag<>;
            }
          } else if constexpr (is_any_capture<T>) {
            // "pass": Output of the `to_capture_ref` branch.
            static_assert(std::is_reference_v<typename T::capture_type>);
            return vtag<safe_alias_of_v<T>>;
          } else {
            // "regular": A non-`capture` binding.
            return vtag<safe_alias_of_v<typename ::folly::bindings::detail::
                                            binding_policy<T>::storage_type>>;
          }
        }())...>{};
  }(TransformedBindingList{});
}

template <typename B>
constexpr bool capture_needs_outer_coro() {
  using BP = ::folly::bindings::detail::binding_policy<B>;
  using ST = typename BP::storage_type;
  return has_async_closure_co_cleanup<ST>;
}

// Converts forwarded arguments to bindings, figures out the storage policy
// (outer coro?, shared cleanup?), and applies `transform_bindings` to
// compute the final storage & binding outcome for each argument.  The
// caller should create an outer coro iff the resulting `std::tuple`
// contains at least one `async_closure_outer_stored_arg`.
//
// Returns a pair:
//   - vtag<safe_alias for non-stored args>
//   - transformed bindings: binding | async_closure_{inner,outer}_stored_arg
template <bool ForceOuterCoro>
constexpr auto transform_capture_bindings(auto&&... args) {
  auto bind_tup = folly::bindings::ensure_binding_tuple(
      std::forward<decltype(args)>(args)...);

  // Future: If there are many `make_in_place` arguments that aren't
  // `as_capture_unique` (and thus require `*capture_heap`), it may be
  // more efficient to auto-select an outer coro with just 2 heap
  // allocations.  Beware: this changes user-facing types (`*capture_heap`
  // to `*capture`), but most users shouldn't depend on this.
  constexpr bool has_outer_coro =
      ForceOuterCoro || []<typename... Bs>(const std::tuple<Bs...>&) {
        // XXX add a test making sure we don't over match on
        // co_cleanup_capture<T&>, as I think we might with the current
        // implementation checking `is_capture_binding<Bs>` would be enough,
        // right? Also test that `as_capture(arg_ref)` fails with a sane
        // message.
        return (capture_needs_outer_coro<Bs>() || ...);
      }(bind_tup);

  // Figure out `IsSharedCleanupClosure` for `binding_cfg` for the real
  // `transform_binding` call.  Whether we guesss `true` or `false` here,
  // this will not affect the presence of `shared_cleanup` in the vtag,
  // since this boolean only picks between `{,body_only_ref_}capture*`, with
  // either `body_only_ref` or `co_cleanup_safe_ref` safety.
  constexpr binding_helper_cfg guess_binding_cfg{
      .is_shared_cleanup_closure = false, .has_outer_coro = has_outer_coro};
  using guess_transformed_binding_types = decltype(std::apply(
      [&](auto... bs) {
        return tag_t<
            decltype(capture_binding_helper<decltype(bs), guess_binding_cfg>::
                         transform_binding(std::move(bs)))...>{};
      },
      std::move(bind_tup)));

  static_assert(
      safe_alias::closure_min_arg_safety == safe_alias::shared_cleanup);
  constexpr binding_helper_cfg binding_cfg{
      // `unsafe` will be forbidden by `async_closure_safe_coro`
      .is_shared_cleanup_closure =
          (safe_alias::shared_cleanup ==
           folly::detail::least_safe_alias( //
               vtag_safety_of_non_stored_args<
                   true,
                   guess_binding_cfg,
                   guess_transformed_binding_types>())),
      .has_outer_coro = has_outer_coro};
  auto res_tup = std::apply(
      [&](auto... bs) {
        return std::tuple{capture_binding_helper<decltype(bs), binding_cfg>::
                              transform_binding(std::move(bs))...};
      },
      std::move(bind_tup));

  if constexpr (guess_binding_cfg == binding_cfg) {
    // This branch is just to avoid the `type_list_concat_t` conversion below.
    return std::pair{
        vtag_safety_of_non_stored_args<
            false,
            binding_cfg,
            guess_transformed_binding_types>(),
        std::move(res_tup)};
  } else {
    return std::pair{
        vtag_safety_of_non_stored_args<
            false,
            binding_cfg,
            type_list_concat_t<tag_t, decltype(res_tup)>>(),
        std::move(res_tup)};
  }
}

} // namespace folly::coro::detail
