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
#include <folly/coro/safe/AsyncClosure.h>
#include <folly/fibers/Semaphore.h>

#ifndef _WIN32 // Explained in SafeTask.h

using namespace folly;
using namespace folly::coro;
using namespace folly::bindings;
using namespace std::literals::chrono_literals;

ClosureTask<int> intTask(int x) {
  co_return x;
}
struct StatelessIntCallable {
  ClosureTask<int> operator()(int x) { co_return x; }
};
struct StatelessGenericCallable {
  ClosureTask<int> operator()(auto x) { co_return x; }
};

// We can't directly test `async_closure*` for unsafe inputs, since that
// would trigger `static_assert`s in `release_outer_coro()`.  Instead, test
// `is_safe()` which verifies the same conditions.
template <bool ForceOuter>
void checkSafety() {
  constexpr int x = 42;

  auto safeWrap = [](auto fn, auto&&... args) {
    return folly::coro::detail::bind_captures_to_closure<ForceOuter>(
        std::move(fn), std::forward<decltype(args)>(args)...);
  };

  // Check safe usage, with various levels of arg safety.
  // Covers: fn ptrs, plain & generic lambdas, callable & generic callables.
  safe_alias_constant<safe_alias::maybe_value> kValue;
  auto checkIsSafe = [&](auto arg_safety, auto fn, auto&&... args) {
    auto s = safeWrap(std::move(fn), std::forward<decltype(args)>(args)...);
    static_assert(s.is_safe());
    static_assert(
        folly::coro::detail::safe_task_traits<
            decltype(std::move(s).release_outer_coro())>::arg_safety ==
        arg_safety.value);
  };
  checkIsSafe(kValue, intTask, 5);
  checkIsSafe(kValue, StatelessIntCallable{}, 5);
  checkIsSafe(kValue, StatelessGenericCallable{}, 5);
  checkIsSafe(kValue, []() -> ClosureTask<int> { co_return 5; });
  checkIsSafe(kValue, []() -> ClosureTask<void> { co_return; });
  checkIsSafe(kValue, [](int x) -> ClosureTask<int> { co_return x; }, 5);
  checkIsSafe(kValue, [](auto) -> ClosureTask<void> { co_return; }, 5);
  checkIsSafe(
      safe_alias_constant<safe_alias::co_cleanup_safe_ref>{},
      [](auto) -> ClosureTask<void> { co_return; },
      manual_safe_ref<safe_alias::co_cleanup_safe_ref>(x));
  checkIsSafe(
      safe_alias_constant<safe_alias::after_cleanup_ref>{},
      [](auto) -> ClosureTask<void> { co_return; },
      manual_safe_ref<safe_alias::after_cleanup_ref>(x));

  auto checkIsUnsafe = [&](auto fn, auto&&... args) {
    auto s = safeWrap(std::move(fn), std::forward<decltype(args)>(args)...);
    static_assert(!s.is_safe());
  };
  // Only `SafeTask` is allowed as the inner coro.
  checkIsUnsafe([]() -> Task<int> { co_return 5; });
  checkIsUnsafe([]() -> Task<void> { co_return; });
  checkIsUnsafe([](int x) -> Task<int> { co_return x; }, 5);
  checkIsUnsafe([](auto) -> Task<void> { co_return; }, 5);
  // Don't allow passing in `unsafe*` args externally.
  checkIsUnsafe(
      [](auto) -> ClosureTask<void> { co_return; },
      manual_safe_ref<safe_alias::unsafe_closure_internal>(x));
}

TEST(AsyncClosure, safetyNoOuter) {
  checkSafety</*force outer*/ false>();
}
TEST(AsyncClosure, safety) {
  checkSafety</*force outer*/ true>();
}

// Checks that `async_closure` returns the `SafeTask` we expect.
template <typename ExpectedT, bool ForceOuter = false>
constexpr auto asyncClosureCheckType(auto&&... args) {
  if constexpr (ForceOuter) {
    auto t =
        async_closure_force_outer_coro(std::forward<decltype(args)>(args)...);
    static_assert(std::is_same_v<decltype(t), ExpectedT>);
    return std::move(t);
  } else {
    static_assert(!ForceOuter);
    auto t = async_closure(std::forward<decltype(args)>(args)...);
    static_assert(std::is_same_v<decltype(t), ExpectedT>);
    return std::move(t);
  }
}

template <bool ForceOuter>
Task<void> checkNoArgs() {
  auto res = co_await asyncClosureCheckType<ValueTask<int>, ForceOuter>(
      []() -> ClosureTask<int> { co_return 7; });
  EXPECT_EQ(7, res);
}

CO_TEST(AsyncClosure, noArgsNoOuter) {
  co_await checkNoArgs</*force outer*/ false>();
}
CO_TEST(AsyncClosure, noArgs) {
  co_await checkNoArgs</*force outer*/ true>();
}

namespace {
static bool ran_returnsVoid;
}

template <bool ForceOuter>
Task<void> checkReturnsVoid() {
  ran_returnsVoid = false;
  co_await asyncClosureCheckType<ValueTask<void>, ForceOuter>(
      []() -> ClosureTask<void> {
        ran_returnsVoid = true;
        co_return;
      });
  EXPECT_TRUE(ran_returnsVoid);
}

CO_TEST(AsyncClosure, returnsVoidNoOuter) {
  co_await checkReturnsVoid</*force outer*/ false>();
}
CO_TEST(AsyncClosure, returnsVoid) {
  co_await checkReturnsVoid</*force outer*/ true>();
}

template <bool ForceOuter>
Task<void> checkPlainArgs() {
  int thirtySix = 36; // test passing l-values
  auto res = co_await asyncClosureCheckType<ValueTask<int>, ForceOuter>(
      [](int x, auto yPtr, const auto z) -> ClosureTask<int> {
        ++x;
        int r = x + *yPtr + z;
        yPtr.reset();
        // Plain args have plain types
        static_assert(std::is_same_v<std::unique_ptr<int>, decltype(yPtr)>);
        co_return r;
      },
      thirtySix,
      std::make_unique<int>(1200),
      100);
  EXPECT_EQ(1337, res);
}

CO_TEST(AsyncClosure, plainArgsNoOuter) {
  co_await checkPlainArgs</*force outer*/ false>();
}
CO_TEST(AsyncClosure, plainArgsOuter) {
  co_await checkPlainArgs</*force outer*/ true>();
}

ClosureTask<std::string> funcTemplate(auto hi) {
  *hi += "de-and-seek";
  co_return std::move(*hi);
}

CO_TEST(AsyncClosure, callFuncTemplate) {
  auto res = co_await asyncClosureCheckType<ValueTask<std::string>>(
      // As of 2024, C++ lacks an "overload set" type, and thus can't
      // directly deduce `funcTemplate` (see P3360R0 pr P3312R0).
      FOLLY_INVOKE_QUAL(funcTemplate),
      as_capture(make_in_place<std::string>("hi")));
  EXPECT_EQ("hide-and-seek", res);
}

// When needed, closure callbacks can have explicit & readable type signatures.
// Unfortunately, the signature depends on whether the closure has an outer
// coro wrapping the inner one.
ClosureTask<std::string> funcNoOuter(capture_heap<std::string> hi) {
  *hi += "de-and-seek";
  co_return std::move(*hi);
}
ClosureTask<std::string> func_with_outer(capture<std::string&> hi) {
  *hi += "de-and-seek";
  co_return std::move(*hi);
}

CO_TEST(AsyncClosure, callFunctionNoOuter) {
  auto res = co_await asyncClosureCheckType<ValueTask<std::string>>(
      funcNoOuter, as_capture(make_in_place<std::string>("hi")));
  EXPECT_EQ("hide-and-seek", res);
}

CO_TEST(AsyncClosure, callFunctionWithOuter) {
  auto res = co_await asyncClosureCheckType<
      ValueTask<std::string>,
      /*force outer*/ true>(
      func_with_outer, as_capture(make_in_place<std::string>("hi")));
  EXPECT_EQ("hide-and-seek", res);
}

CO_TEST(AsyncClosure, simpleCancellation) {
  EXPECT_THROW(
      co_await timeout(
          async_closure([]() -> ClosureTask<void> {
            folly::fibers::Semaphore stuck{0}; // a cancellable baton
            co_await stuck.co_wait();
          }),
          200ms),
      folly::FutureTimeout);
}

struct InPlaceOnly : folly::NonCopyableNonMovable {
  explicit InPlaceOnly(bool* made, int n) : n_(n) {
    if (made) {
      *made = true;
    }
  }
  int n_;
};

void assertArgConst(auto& arg) {
  static_assert(std::is_const_v<std::remove_reference_t<decltype(*arg)>>);
  static_assert(
      std::is_const_v<std::remove_pointer_t<decltype(arg.operator->())>>);
}

template <bool ForceOuter>
Task<void> checkInPlaceArgs() {
  bool made = false;
  auto res = co_await asyncClosureCheckType<ValueTask<int>, ForceOuter>(
      [](int a, auto b, auto c, auto d) -> ClosureTask<int> {
        static_assert(
            std::is_same_v<
                decltype(b),
                std::conditional_t<ForceOuter, capture<int&>, capture<int>>>);
        *b += 100;
        static_assert(std::is_same_v<
                      decltype(c),
                      std::conditional_t<
                          ForceOuter,
                          capture<const InPlaceOnly&>,
                          capture_heap<const InPlaceOnly>>>);
        assertArgConst(c); // `const` underlying type
        assertArgConst(d); // marked `constant`
        co_return a + *b + c->n_ + *d;
      },
      30, // a
      // Test both const and non-const `AsyncOuterClosurePtr`s
      as_capture(1000), // b
      as_capture(make_in_place<const InPlaceOnly>(&made, 7)), // c
      as_capture(constant(200))); // d
  EXPECT_EQ(1337, res);
  EXPECT_TRUE(made);
}

CO_TEST(AsyncClosure, inPlaceArgsNoOuter) {
  co_await checkInPlaceArgs</*force outer*/ false>();
}
CO_TEST(AsyncClosure, inPlaceArgs) {
  co_await checkInPlaceArgs</*force outer*/ true>();
}

// Tests that, with an outer coro, the user can specify `const auto`
// args on the inner task, and they work as expected.
//
// IIUC this can't work generically for the "no outer coro" scenario, since
// args need to be copied or moved into the inner coro, and non-copyable,
// `const` classes are not movable.  In `checkInPlaceArgs()`, you can see
// the workaround of passing a `const` (or equivalenly `constant()`) arg.
CO_TEST(AsyncClosureTest, constAutoArgWithOuterCoro) {
  bool made = false;
  auto res = co_await asyncClosureCheckType<
      ValueTask<int>,
      /*force outer*/ true>(
      [](const auto a) -> ClosureTask<int> {
        static_assert(
            std::is_same_v<decltype(a), const capture<const InPlaceOnly&>>);
        assertArgConst(a);
        co_return a->n_;
      },
      as_capture(make_in_place<
// Manual test: this should fail to compile without the `const` because the
// `const auto` forces (via `FOLLY_MOVABLE_AND_DEEP_CONST_LREF_COPYABLE`)
// the inner type to be const.
#if 1
                 const
#endif
                 InPlaceOnly>(&made, 7)));
  EXPECT_EQ(7, res);
  EXPECT_TRUE(made);
}

// A simple test pair showing the "move-in" vs "by-ref" behavior of the "no
// outer coro" optimization. The `nestedRefs*` tests elaborate on this.
CO_TEST(AsyncClosure, noOuterCoroNoArgRef) {
  co_await async_closure(
      [](auto n) -> ClosureTask<void> {
        static_assert(std::is_same_v<decltype(n), capture<int>>);
        co_return;
      },
      as_capture(1337));
}
CO_TEST(AsyncClosure, outerCoroHasArgRef) {
  co_await async_closure_force_outer_coro(
      [](auto n) -> ClosureTask<void> {
        static_assert(std::is_same_v<decltype(n), capture<int&>>);
        co_return;
      },
      as_capture(1337));
}

CO_TEST(AsyncClosure, nestedRefsWithOuterCoro) {
  auto res =
      co_await asyncClosureCheckType<ValueTask<int>, /*force outer*/ true>(
          [](auto x, const auto y, const auto z) -> ClosureTask<int> {
            static_assert(std::is_same_v<decltype(x), capture<int&>>);
            static_assert(std::is_same_v<
                          decltype(y),
                          const capture<const std::unique_ptr<int>&>>);
            assertArgConst(y);
            static_assert(std::is_same_v<
                          decltype(z),
                          const capture_indirect<const std::unique_ptr<int>&>>);
            *x += 100;
            co_await asyncClosureCheckType<CoCleanupSafeTask<void>>(
                [](auto x2, auto y2, auto z2) -> ClosureTask<void> {
                  static_assert(std::is_same_v<decltype(x2), capture<int&>>);
                  static_assert(std::is_same_v<
                                decltype(y2),
                                capture<const std::unique_ptr<int>&>>);
                  assertArgConst(y2);
                  static_assert(std::is_same_v<
                                decltype(z2),
                                capture_indirect<const std::unique_ptr<int>&>>);
                  *x2 += 100; // ref remains non-const -- C++ arg semantics
                  co_return;
                },
                x,
                y,
                z);
            // Can also pass `capture<Ref>`s into a bare SafeTask.
            co_await [](auto x3, auto y3, auto z3) -> CoCleanupSafeTask<void> {
              static_assert(std::is_same_v<decltype(x3), capture<int&>>);
              static_assert(std::is_same_v<
                            decltype(y3),
                            capture<const std::unique_ptr<int>&>>);
              assertArgConst(y3);
              static_assert(std::is_same_v<
                            decltype(z3),
                            capture_indirect<const std::unique_ptr<int>&>>);
              *x3 += 100; // ref remains non-const -- C++ arg semantics
              co_return;
            }(x, y, z);
            co_return *x + **y + *z;
          },
          as_capture(
              make_in_place<int>(1000), constant(std::make_unique<int>(23))),
          as_capture_indirect(constant(std::make_unique<int>(14))));
  EXPECT_EQ(1337, res);
}

// We want this to be as similar as possible to `nestedRefsWithOuterCoro` --
// after all, "no outer coro" is supposed to be a "mostly transparent"
// optimization. Therefore, the main differences are:
//   - `capture`s move into the inner coro, and therefore cannot:
//     * Write `const auto y` or `const auto z`, which would need a copy ctor
//     * Use `constant()` around `std::make_unique()` (prevents move).
//   - Correspondingly, we have to drop the `const`ness asserts.
//   - To pass `capture<Val>` into a bare `SafeTask`, we now have to
//     explicitly declare the its argument types, to use the implicit
//     conversion from `capture<Val>` to `capture<Val&>`.
CO_TEST(AsyncClosure, nestedRefsWithoutOuterCoro) {
  auto res = co_await asyncClosureCheckType<
      ValueTask<int>,
      /*force outer*/ false>(
      [](auto x, auto y, auto z) -> ClosureTask<int> {
        static_assert(std::is_same_v<decltype(x), capture_heap<int>>);
        static_assert(std::is_same_v<
                      decltype(z),
                      capture_indirect<std::unique_ptr<const int>>>);
        *x += 100;
        co_await asyncClosureCheckType<CoCleanupSafeTask<void>>(
            [](auto x2, auto y2, auto z2) -> ClosureTask<void> {
              static_assert(std::is_same_v<decltype(x2), capture<int&>>);
              static_assert(
                  std::is_same_v<decltype(y2), capture<std::unique_ptr<int>&>>);
              static_assert(std::is_same_v<
                            decltype(z2),
                            capture_indirect<std::unique_ptr<const int>&>>);
              *x2 += 100; // ref remains non-const -- C++ arg semantics
              co_return;
            },
            x,
            y,
            z);
        // Can pass implicitly converted `capture<Ref>`s into a SafeTask
        co_await [](capture<int&> x3,
                    capture<std::unique_ptr<int>&> y3,
                    capture_indirect<std::unique_ptr<const int>&>)
                     -> CoCleanupSafeTask<void> {
          *x3 += 50;
          *(*y3) += 50;
          co_return;
        }(x, y, z);
        co_return *x + **y + *z;
      },
      as_capture(make_in_place<int>(1000), std::make_unique<int>(23)),
      // Can't use `constant()` here because we can't move a `const unique_ptr`.
      as_capture_indirect(std::make_unique<const int>(14)));
  EXPECT_EQ(1337, res);
}

struct ErrorObliviousHasCleanup {
  explicit ErrorObliviousHasCleanup(int* p) : cleanBits_(p) {}
  int* cleanBits_;
  auto co_cleanup(async_closure_private_t) {
    return std::tuple{
        [this]() -> Task<void> {
          *cleanBits_ += 1;
          co_return;
        }(),
        [this]() -> Task<void> {
          *cleanBits_ += 2;
          co_return;
        }()};
  }
};

CO_TEST(AsyncClosure, errorObliviousCleanup) {
  int cleanBits = 0;
  co_await async_closure(
      [](auto) -> ClosureTask<void> { co_return; },
      as_capture(ErrorObliviousHasCleanup{&cleanBits}));
  EXPECT_EQ(3, cleanBits);
}

struct MagicError : std::exception {
  explicit MagicError(int m) : magic_(m) {}
  int magic_;
};

struct HasCleanup {
  explicit HasCleanup(auto* p) : optCleanupErrPtr_(p) {}
  std::optional<exception_wrapper>* optCleanupErrPtr_;
  int magicToThrow_{0}; // 0 for cleanup not to throw, otherwise MagicError{}
  // If the closure (not other cleanups!) exited with an exception, each
  // `co_cleanup` gets to see it.
  folly::coro::Task<void> co_cleanup(
      async_closure_private_t, const exception_wrapper* ew) {
    *optCleanupErrPtr_ = *ew;
    if (magicToThrow_) {
      throw MagicError{magicToThrow_};
    }
    co_return;
  }
};

CO_TEST(AsyncClosure, cleanupAfterSuccess) {
  std::optional<exception_wrapper> optCleanErr;
  co_await async_closure(
      [](auto) -> ClosureTask<void> { co_return; },
      as_capture(HasCleanup{&optCleanErr}));
  EXPECT_FALSE(optCleanErr->has_exception_ptr());
}

CO_TEST(AsyncClosure, cleanupAfterError) {
  std::optional<exception_wrapper> optCleanErr;
  auto res = co_await co_awaitTry(async_closure(
      [](auto) -> ClosureTask<void> {
        co_yield folly::coro::co_error{MagicError{111}};
      },
      as_capture(HasCleanup{&optCleanErr})));
  EXPECT_EQ(111, optCleanErr->get_exception<MagicError>()->magic_);
  EXPECT_EQ(111, res.tryGetExceptionObject<MagicError>()->magic_);
}

CO_TEST(AsyncClosure, cleanupThrows) {
  std::optional<exception_wrapper> x1, x2, x3, x4;
  auto res = co_await co_awaitTry(async_closure(
      [](auto, auto a2, auto a3, auto) -> ClosureTask<void> {
        a2->magicToThrow_ = 37;
        a3->magicToThrow_ = 42;
        co_return;
      },
      as_capture(HasCleanup{&x1}),
      as_capture(HasCleanup{&x2}),
      as_capture(HasCleanup{&x3}),
      as_capture(HasCleanup{&x4})));
  // All four cleanups ran, and saw no error, despite two of them throwing.
  EXPECT_FALSE(x1->has_exception_ptr());
  EXPECT_FALSE(x2->has_exception_ptr());
  EXPECT_FALSE(x3->has_exception_ptr());
  EXPECT_FALSE(x4->has_exception_ptr());
  // We get one of the exceptions back.
  auto* ex = res.tryGetExceptionObject<MagicError>();
  EXPECT_TRUE(ex->magic_ == 42 || ex->magic_ == 37);
}

// XXX nest with cleanup

struct CustomDerefCleanupProxy : NonCopyableNonMovable {
  explicit CustomDerefCleanupProxy(int y) : y_(y) {}
  auto operator->() { return static_cast<CustomDerefCleanupProxy*>(this); }
  int y_;
};

struct CustomDerefCleanup : HasCleanup {
  explicit CustomDerefCleanup(auto* p) : HasCleanup(p) {}
  friend auto capture_lref_proxy(
      ::folly::coro::detail::capture_private_t, CustomDerefCleanup&) {
    return CustomDerefCleanupProxy{101};
  }
  friend auto capture_ptr_proxy(
      ::folly::coro::detail::capture_private_t, CustomDerefCleanup&) {
    return CustomDerefCleanupProxy{202};
  }
};

template <typename CleanupT>
Task<void> check_pass_cleanup_arg_to_subclosure() {
  std::optional<exception_wrapper> optCleanErr;
  co_await async_closure(
      [](auto c) -> ClosureTask<void> {
        static_assert(
            std::is_same_v<decltype(c), co_cleanup_capture<CleanupT&>>);
        co_await async_closure(
            [](auto c2) -> ClosureTask<void> {
              static_assert(
                  std::is_same_v<decltype(c2), co_cleanup_capture<CleanupT&>>);
              co_return;
            },
            c);
      },
      as_capture(CleanupT{&optCleanErr}));
  EXPECT_FALSE(optCleanErr->has_exception_ptr());
}

CO_TEST(AsyncClosure, passCleanupArgToSubclosure) {
  co_await check_pass_cleanup_arg_to_subclosure<HasCleanup>();
}
// Check that the "custom dereferencing" code doesn't break the automatic
// passing of `capture` refs to child closures.
CO_TEST(AsyncClosure, passCustomDerefCleanupArgToSubclosure) {
  co_await check_pass_cleanup_arg_to_subclosure<CustomDerefCleanup>();
}

/*

XXX test "no stored args", with and without "force outer closure"

XXX test matrix -- cover more of this in a Bindings.h test
outer: ref to stored, ref to ancestor, plain
inner: stored, ref to ancestor, plain
cover lval and rval refs

XXX test shared_cleanup downgrade, making after_cleanup_ref_ args

XXX named type sigs should support defaults soonish
  => orthogonal to namedness, really
XXX also, can combine pos & kw (pos1, pos2, "x"_id=4, "y"_id=5)
    with the trailing kwargs getting rolled up into one
    trailing scope

*/

TEST(AsyncClosure, nonSafeTaskIsNotAwaited) {
  bool awaited = false;
  auto lambda = [&]() -> Task<void> {
    awaited = true;
    co_return;
  };
  // We can't `release_outer_coro()` on either since they have a
  // `static_assert` -- but `checkIsUnsafe` above checks the logic.
  folly::coro::detail::bind_captures_to_closure</*force outer*/ false>(lambda);
  folly::coro::detail::bind_captures_to_closure</*force outer*/ true>(lambda);
  EXPECT_FALSE(awaited);
}

struct HasMemberTask {
  MemberTask<int> task(auto x) { co_return 1300 + x; }
};

CO_TEST(AsyncClosure, memberTask) {
  EXPECT_EQ(
      1337,
      co_await async_closure(
          [](auto mt) -> ClosureTask<int> {
            co_return co_await async_closure(FOLLY_INVOKE_MEMBER(task), mt, 37);
          },
          as_capture(HasMemberTask{})));
}

CO_TEST(AsyncClosure, nowClosure) {
  int m = 37;
  auto fn = [&]() {
    return async_now_closure(
        [](auto n, int& m) -> Task<int> {
          static_assert(std::is_same_v< // No ref upgrade
                        after_cleanup_capture<int>,
                        decltype(n)>);
          co_return *n + m;
        },
        as_capture(1300),
        m);
  };
  EXPECT_EQ(1337, co_await fn());
}

CO_TEST(AsyncClosure, nowClosureCoCleanup) {
  std::optional<exception_wrapper> optCleanErr;
  auto fn = [&]() {
    return async_now_closure(
        [](auto cleanup, auto n) -> Task<int> {
          static_assert(std::is_same_v<
                        co_cleanup_capture<HasCleanup&>,
                        decltype(cleanup)>);
          static_assert(std::is_same_v< // No ref upgrade
                        after_cleanup_capture<int&>,
                        decltype(n)>);
          co_return *n + 37;
        },
        as_capture(HasCleanup{&optCleanErr}),
        as_capture(1300));
  };
  EXPECT_FALSE(optCleanErr.has_value());
  EXPECT_EQ(1337, co_await fn());
  EXPECT_TRUE(optCleanErr.has_value());
}

#endif
