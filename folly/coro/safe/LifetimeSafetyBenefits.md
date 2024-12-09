## Purpose

Start with the overview in `SaferCoro.md`, and glance at `AsyncClosure.md`.

Below, we'll focus on this overarching question:

> Why introduce extra complexity for lifetime safety?

If you're interested in *how* lifetime safety works, first look through this
doc to get a sense of the bugs we want to catch, and **then** read
`LifetimeSafetyDesign.md`.

Lifetime safety is primarily enforced through `SafeTask.h`, which has
working-programmer docs inline.  However, if you just want to write code,
read `APIBestPractices.md` -- that helps you pick the right `Task` wrapper.

Some safety benefits of using `async_closure` / `SafeAsyncScope` /
`AsyncScopeObject` are logically independent of the lifetime-safety machinery:
  - Exception safety: Async scope `join` is always triggered.
  - `AsyncScopeObject` & friends are better than the manual `co_await
    obj.cleanup()`, or the deadlock-prone `~Obj() { blockingWait(cleanup()); }`.
  - Async scope cancellation correctness: `CancelViaParent` will request
    cancellation for scope tasks iff the parent coro is cancelled. Other
    easy-to-use policies are provided.

Despite their logical independence, as of today, those tools require `SafeTask`,
and/or lifetime-marked `capture` wrappers in their critical APIs. A sub-goal of
this document is to give some justification to coupling lifetime safety with
the other concerns.

The following docs address related topics, but are not required to read the
present one:
  - `SafeAliasAndClosure.md` details the reference-safety hierarchy
  - `Captures.md` to understand the nitty-gritty of this type wrapper family
  - `SafeAsyncScope.md` and `AsyncObject.md` for those specific APIs

## Use `NowTask` first, `SafeTask` last

Most of the complexity you need is "3 extra letters per coro": `Task` ->
`NowTask`.

Do not read further until you believe that `NowTask` is the right default for
90%+ of usage. Review the primer on [structured
concurrency](https://ericniebler.com/2020/11/08/structured-concurrency/).

In short, when an async task is **created & awaited in the same expression**,
regular C++ lexical scope and reference lifetime extensions rules will prevent
most "dangling reference" bugs that afflict less-structured concurrency. E.g.
  - `co_await foo()`
  - `co_await collectAll(bar(), baz())`

You can also write `async_now_closure([](...) -> Task<void> {}, ...)` and get
back a `NowTask` with exception-safe and cancellation-correct async RAII, and a
similar lack of async-specific lifetime bugs.

The rest of this document will tackle the most treacherous parts of async
programming, which can come up when adding structure to an unstructured
codebase, or when writing complex programs:
  - Deferring `co_await` due to conditional control flow, or due to wanting to
    front-load allocations.
  - Creating async tasks dynamically, outside of the standard structured
    patterns.
  - Scope-local scatter-gather usage of "async scopes" -- use `SafeAsyncScope`,
    instead of vanilla `AsyncScope`.
  - Managing classes with "background async work" -- use `AsyncScopeObject` in
    most cases, or `AsyncScopeSlotObject` / `AsyncObject` for power users.

## `async_closure()` makes `SafeTask` or `NowTask`, automagically

The next 5 lines are equivalent at runtime, *unless* you pass arguments
requiring async RAII, such as `safeAsyncScope<CancelViaParent>()`, which
requires `async_closure`.

```cpp
// Await a SafeTask instance
co_await async_closure([](...) -> ClosureTask<T> {/*body*/}, ...);
co_await [](...) -> SafeTask<Safety, T> {/*body*/}(...);

// Await a non-movable NowTask instance
co_await async_now_closure([](...) -> Task<T> {/*body*/}, ...);
co_await [](...) -> NowTask<T> {/*body*/}(...);

// Await an vanilla, unchecked Task
co_await [](...) -> Task<T> {/*body*/}(...);
```

The point is, `async_closure` isn't just for async RAII, it is a general tool
for instantiating tasks with better lifetime safety. Its only downsides are
code length & compiler cycles. But, if you:
  - want a movable task,
  - with lifetime safety checks, and
  - don't want to figure out the right `SafeTask` via `APIBestPractices.md`

... then, `async_closure()` is your ticket. There is no runtime cost.

If the name feels too long, add an alias in your `.cpp` file:

```cpp
namespace {
inline constexpr auto ac = folly::coro::async_closure;
}
```

## A recap of `SafeTask`

### What is it? (from the header)

`SafeTask` is a thin wrapper around `folly::coro::Task` that uses
`safe_alias_of_v` to enforce some compile-time guarantees:
  - The `SafeTask` has `safe_alias_of_v` memory safety at least as high as the
    coro's arguments. In particular, no args are taken by reference.
  - Regardless of the task's declared safety, the coro's return must have safety
    `maybe_value` (explained in `SafeTaskRetAndArgs`).
  - The coroutine is **not** a stateful callable -- this prohibits lambda
    captures, since those are a very common cause of coro memory bugs.

### What complexity does it introduce?

In a nutshell: new concepts, vocabulary, and idioms. The required patterns are
inherently safer, but come with a learning curve.

  - Since you can't use by-reference args, or reference captures, you end up
    needing to learn `safe/coro`-specific concepts & vocabulary such as
    `as_capture()`, `capture_ref()`, etc.

    Unfortunately, per `RelationToWGProposals.md`, it will be a while before the
    core language will be able to transparently enforce analogous safety rules.

  - Replacing lambda captures by arguments is more verbose.

  - While using `async_closure()` auto-selects the right `SafeTask` based on its
    inputs, there are cases where users -- and especially library authors --
    will need to look at `APIBestPractices.md` or even `SafeAliasAndClosure.md`
    to figure out the right "safety level".

## Why you might use `SafeTask` anyway?

In short, because the user-facing vocabulary isn't enormous, `auto` hides much
of it, and it is only needed for complex use-cases. At runtime, `SafeTask`
should be zero-cost. In terms of complexity, "you pay for what you use" -- and
your usage will be shorter & clearer than the equivalent "manually safe" code.

Above, we narrowed the domain of `SafeTask` to "complex asynchrony". Complexity
usually means a higher risk of bugs, and lifetime bugs are one of the worst
kinds, because they can lead to memory corruption, unpredictable hard-to-debug
crashes, and even security exploits.

### Start by using sanitizers, warnings, and linters!

Linters are great, use them! For example, `folly/coro/safe` relies on having an
active use-after-move linter, like the one from `clang-tidy`.

Enable all the warnings you practically can (`-Wall`, `-Werror`, maybe
`-Wextra`, and selectively disable backwards-compat or low-ROI ones). For
example, you should definitely use the new [clang warnings](
https://discourse.llvm.org/t/lifetime-analysis-improvements-in-clang/81374).
But, their applicability is limited. Even analysis of
`[[clang::lifetimebound]]`-enabled APIs is quite coarse.

The best hope for lifetime analysis is that Herb Sutter's [lifetime safety
profile P1179R1](https://wg21.link/P1179R1) is accepted and implemented, **and**
that it then gets extended to handle async lifetime issues. Until then, the
`SafeTask` / `capture` mechanism will remain highly relevant.

In addition to using `SafeTask`, you should definitely test your product under
ASAN and all the other sanitizers you can afford.

Static checks and runtime sanitizers have complementary strengths, and neither
replaces the other. Furthermore, understanding and fixing an ASAN failure is
usually more time-consuming than fixing a compiler error, which tells you
exactly what is wrong, and on what line. `coro/safe` error are intended to meet
that "easy troubleshooting" standard. The library already has a good number of
diagnostic `static_assert`s and C++20 constraints, and more will be added as
user reports come in.

### Sanitizers won't catch lifetime bugs on rare code paths

One of the most troublesome kinds of bugs -- and one that ASAN is unlikely to
catch -- is "lifetime conditional on uncommon code paths". For example, in this
legacy `folly::coro` code, an exception will cause an invalid reference to be
accessed, if background tasks are still running.

```cpp
AsyncScope scope;
WorkContext ctx;
while (co_await ctx.hasMoreTasks()) {
  scope.add((co_await ctx.nextTask()).scheduleOn(co_await co_current_executor));
}
co_await scope.joinAsync(); // BAD: exception-unsafe
```

This naive code will run happily under ASAN, but if `nextTask()` throws, this
will expose two latent bugs:
  - *(lifetime)* `scope` outlives `ctx` => dangling ref heisenbugs,
  - *(no async RAII)* `scope` isn't joined on error => `~AsyncScope` will
    `LOG(FATAL)`.

There are subtler issues too -- (1) cancellation isn't propagated, (2) even with
a try-catch-store-`exception_ptr` pattern, a `bad_alloc` from the
`joinAsync()` instantiation will still fatal in `~AsyncScope`.

In contrast, with `folly/coro/safe`, this works today:

```cpp
co_await async_closure([](auto scope, auto ctx) -> ClosureTask<void> {
      while (co_await ctx->hasMoreTasks()) {
        scope.with(co_await co_current_executor).schedule(
            async_closure(FOLLY_INVOKE_MEMBER(nextTask), ctx));
      }
    },
    safeAsyncScope<CancelViaParent>(), // GOOD: RAII cleanup
    as_capture(make_in_place<WorkContext>()));
```

A small `FutureWork.md` feature will let us write this more naturally:

```cpp
WorkContext ctx;
co_await async_closure([](auto scope, auto ctx) -> ClosureTask<void> {
   // ... ditto ...
   capture_ref(ctx));
```

*Aside:* You can find an ready-to-run example similar to this snippet in
`examples/CollectAllWindowedAsync.cpp`.

## Safety cost-benefit analysis

Most of the above magic is **not** from `SafeTask`:
  - The use of `async_closure()` gives us async RAII with exception-safety,
    including `bad_alloc` correctness.
  - The closure cooperates with `SafeAsyncScope<CancelViaParent>` to propagate
    cancellation via the `setParentCancelToken` protocol.

So, you could imagine rewriting this with a traditional lambda capture:

```cpp
WorkContext ctx;
co_await async_now_closure([&ctx](auto scope) -> Task<void> {
      while (co_await ctx.hasMoreTasks()) {
        // NOTE: There's no `scheduleUnsafe()`, read on to see why.
        scope.with(co_await co_current_executor).scheduleUnsafe(
            ctx.nextTask());
      }
    }, safeAsyncScope<CancelViaParent>());
```

The above is correct and shorter, but `scheduleUnsafe` is not yet provided. Why
is that? Let's first see if the safer API's benefits justify its weight.

### The cost of safety

First, the "safer" version binds and accesses `ctx` differently:
  - `[&ctx]` becomes `(auto ctx)` + `capture_ref(ctx)`. Longer, but still
    "normal C++".
  - `ctx.` becomes `ctx->`. This is expected if the user has seen `capture`s
    before.

What users need to know about `capture` wrappers can be largely reduced to:
  - `capture`s need to be dereferenced.
  - After dereferencing, `capture<T>` acts like `T` for all value categories.

    *Caveat*: `capture<V&&>` enforces stricter "single use" rules to reduce
    use-after-move bugs. No need to worry, typical correct code will just work.
  - Pass `capture`s by `auto` to avoid caring about the exact type. If you need
    an exact type, let the compiler error tell you :)
  - **(safety magic here)** `capture<>`s never move from the owning closure --
    if you pass one to a child task, it is *always* taken by-reference, and the
    reference is verified-safe by the compiler.

    For example, `capture<int> a` passed as `async_closure([](auto a) ..., a)`
    becomes `capture<int&>`.

*Aside*: The adoption cost of `capture` is shared with the "async RAII"
implementation. The closure has to own the object, for which it guarantees
`co_cleanup`. The `capture` wrappers were in part written to store such objects,
and to expose them by reference, while discouraging users from moving them out
from the owning closure (a serious & non-obvious bug). That said, if we didn't
care about API safety at all (like `co_scope_exit` today), we could pass a raw
`CleanableAsyncScope&` into the closure's inner task, and `capture` types could
be eliminated.

Second, this member invocation is **weird**:

```cpp
async_closure(FOLLY_INVOKE_MEMBER(nextTask), ctx)
// Sugar for
async_closure([](auto ctx) { return ctx->nextTask(); }, ctx)
```

The reason for the wart is explained in the `MemberTask` comment in
`SafeTask.h`, and might go away with [P3312](https://wg21.link/P3312). It only
applies to member coros, whose validity is limited by the lifetime of `this`.
With free, or static functions you don't *have* to use `async_closure` or the
macro -- you can also just use `CoCleanupSafeTask` as your coroutine type.

### Safety benefits: Examples

Safety is a funny beast -- you won't see it in correct code that compiles!
Instead, the whole point is that **incorrect code fails to compile**.

The goal of `capture` & `schedule(async_closure())` in the prior section is to
construct a `CoCleanupSafeTask`, which takes `ctx` by reference -- yet
guarantees that the `ctx` will outlive the `scope`. If you're curious how,
check out `LifetimeSafetyDesign.md`.

But first, a non-exhaustive list of bugs that `folly/coro/safe` can prevent.

#### Example: Reference to loop variable

This is both wrong and sporadically memory-unsafe if `i` is taken by-reference,
but such a bug is impossible with `SafeAsyncScope`'s current `schedule()` API.

```cpp
Task<void> WorkContext::nextTask(const int& n); // BAD!
int i = 0;
while (co_await ctx.hasMoreTasks()) {
  scope.with(co_await co_current_executor).scheduleUnsafe(
      ctx.nextTask(++i));
}
```

Here's a trickier version of this bug from [Francesco Zoffoli's CppCon2023
talk](https://youtu.be/Iqrd9vsLrak):

```cpp
vector<Task<T>> tasks;
for (int index = …) {
  Foo foo{index};
  tasks.push_back(foo.bar()); // BAD: `this` will be invalid
}
co_await collectAll(tasks);
```

Since `folly/coro/safe` recommends that `bar()` be `NowTask` or `MemberTask`.
Neither would compile. But with `MemberTask`, the following loop inner body
would work correctly. If you want it to be shorter, check out `ac_`-prefixed
methods in `APIBestPractices.md`.

```cpp
tasks.push_back(async_closure(
    FOLLY_INVOKE_MEMBER(bar), as_capture(Foo{index})));
// NB: If non-movable, use `as_capture(make_in_place<Foo>(index))`
```

#### Example: Ephemeral lambda with captures

The verbosity of replacing `[]` C++ capture syntax by coro args is easy to
justify. Despite knowing better, the original author of `folly/coro/safe`
routinely writes bugs of this nature:

```cpp
int x = magicNumber();
auto xPlus7 = [x]() -> Task<int> { co_return x + 7; }(); // BAD!
if (something()) {
  co_await orOther(std::move(xPlus7));
} else {
  co_await xPlus7;
}
```

Oops. The original lambda -- and its value capture -- is long-destroyed by the
time `xPlus7` is awaited. ASAN catches this, but the error takes valuable
minutes to interpret. `co_invoke` prevents it, but brings runtime overhead. In
contrast, none of the `folly/coro/safe` options would compile (`NowTask`,
`SafeTask`, `async_closure`, or `async_now_closure`), and all of them are
zero-cost.

#### Example: Ephemeral objects / invalid `this`

This bug is the more-obvious analog of the "emphemeral lambda" example. Using
the `folly/coro/safe`-recommended `NowTask` or `MemberTask` also prevents it:

```cpp
Task<int> t = Foo{}.bar(); // BAD: `this` will be invalid
...
co_await std::move(t);
```

#### Example: Pop quiz!

This same bug appears in prior examples, but it *looks* a bit different here. Can you spot it?

```cpp
// BAD: Where is the bug?
Task<void> addWork(AsyncScope& scope, int n) {
  scope.add(co_withCancellation(
      co_await co_current_cancellation_token,
      [n]() -> Task<void> {
        std::cout << "task " << n << std::endl;
        co_return;
      }().schedule(co_await co_current_executor)));
}
AsyncScope scope;
auto joinScope = scope.joinAsync(); // Preallocate coro for `bad_alloc` safety
try {
  co_await addWork(scope, 1);
  co_await addWork(scope, 2);
} catch (...) {
  // Real code might save std::current_exception, but we cannot
  // let it fly before joining the scope.
}
co_await std::move(joinScope);
```

This fancy-looking code fixes the RAII-related problems of our very first
`AsyncScope` example:
  - It takes care to join the scope in an exception-safe way.
  - It propagates cancellation from the outer coro to the on-scope tasks.

But, all this machinery hides that... the lambda capturing `n` is destroyed at
the semicolon. The same issue would occur if the task took a reference to any
local in the `addWork` scope. Lifetime problems almost always occur because that
an async scope, or another move-and-await extends the lifetime of the task past
its natural lexical scope.

Time for a safety upgrade.
  - Let's write `NowTask addWork`, which isn't a bug here, but reassures us in
    the future the `scope` ref is valid.
  - Let's use `SafeAsyncScope`, which can only schedule `CoCleanupSafeTask` or
    `ValueTask`.

Let's try this:

```cpp
NowTask<void> addWork(auto& scope, int n) {
  scope->with(co_await co_current_executor)
      .schedule(async_closure(
          [n]() -> Task<void> {
            std::cout << "task " << n << std::endl;
            co_return;
          }()));
}
co_await async_closure([](auto scope) -> ClosureTask<void> {
  co_await addWork(scope, 1);
  co_await addWork(scope, 2);
}, safeAsyncScope<CancelViaParent>());
```

Oh, *oops*, that didn't compile because `async_closure` requires a `ClosureTask`
or another `SafeTask`. A `NowTask` from `async_now_closure` can't be scheduled
on `SafeAsyncScope`. If I try `ClosureTask<void>` for the lambda, that fails
with a "Bad SafeTask" compile error that explains you can't have stateful
callables, like captures. Finally, I see the the bug, and change the inner
lambda to:

```cpp
[](int n) -> ClosureTask<void> {
  std::cout << "task " << n << std::endl;
  co_return;
}(n)
```

So, the async RAII machinery made the code shorter while retaining
exception-safety & cancellation-correctness, and the lifetime safety tooling
stopped me from compiling buggy code. That's a win, right?

#### Bonus example: Two async scopes

This scenario does not occur in your average program, but it shows that our
lifetime checks are quite robust. Our goal is to run some "mandatory" and "best
effort" work concurrently, and to cancel leftover "best effort" tasks once the
"mandatory" ones finish. To simplify the classic `folly::coro` implementation,
we will **not** cancel required work if the outer coro is canceled -- although
with `SafeAsyncScope` this would be a 1-word change.

```cpp
// BAD: Can you find a tricky lifetime bug below?
Task<void> schedule(auto& scope, Task<void> item) {
  scope.add(std::move(item).scheduleOn(co_await co_current_executor));
}
Task<void> doWork(WorkItem)  { /*...*/ }
Task<void> doOptionalWork(AsyncScope& requiredScope, WorkItem item) {
  if (co_await item.isRequired()) { // Last-minute escalation?
    requiredScope.add(
        doWork(std::move(item))
            .scheduleOn(co_await co_current_executor));
  } else {
    co_await doWork(std::move(item));
  }
}

AsyncScope requiredScope(/*throwOnError*/true);
// Best-effort tasks, will be canceled after `requiredScope` completes.
CancellableAsyncScope optionalScope(/*throwOnError*/true);

// Pre-allocate "join"s to guarantee cleanup on `bad_alloc`
auto tryJoinRequired = co_awaitTry(requiredScope.joinAsync());
auto tryCancelAndJoinOptional =
    co_awaitTry(optionalScope.cancelAndJoinAsync());
try {
  co_await schedule(requiredScope, doWork(iAmMandatory()));
  co_await schedule(requiredScope, doWork(nonNegotiable()));
  co_await schedule(
      optionalScope, doOptionalWork(requiredScope, bigwigInterest()));
  co_await schedule(
      optionalScope, doOptionalWork(requiredScope, canDoThisLater()));
} catch (...) {
  // ... TODO: save & handle exception AFTER joining scopes
}
auto joinTry1 = co_await std::move(tryJoinRequired);
auto joinTry2 = co_await std::move(tryCancelAndJoinOptional);
```

This program looks pretty competently written, right? It handles errors from
scope tasks (which normally crash your program), implements meticulous
exception-safety around scope joining, and does what was promised with regards
to cancellation. So, what's the catch?

The bug will be revealed soon. First, look at the logically equivalent safer
code. It is "just as correct", except that the bug is now a compile error.

```cpp
CoCleanupSafeTask<void> doWork(WorkItem)  { /*...*/ }
CoCleanupSafeTask<void> doOptionalWork(
    co_cleanup_capture<SafeAsyncScope<NeverCancel>&> requiredScope,
    WorkItem item) {
  if (co_await item.isRequired()) { // Last-minute escalation?
    requiredScope->with(co_await co_current_executor).schedule(
        doWork(std::move(item)));
  } else {
    co_await doWork(std::move(item));
  }
}

co_await async_closure([](auto requiredScope, auto optionalScope)
        -> ClosureTask<void> {
      const auto& exec = co_await co_current_executor;
      requiredScope->with(exec).schedule(doWork(iAmMandatory()));
      requiredScope->with(exec).schedule(doWork(nonNegotiable()));
      optionalScope->with(exec).schedule(
          doOptionalWork(requiredScope, bigwigInterest()));
      optionalScope->with(exec).schedule(
          doOptionalWork(requiredScope, canDoThisLater()));
    },
    safeAsyncScope<NeverCancel>(), // Match "classic". Or: `CancelViaParent`
    safeAsyncScope<CancelOnExitOrRequest>());
```

Today's compiler error says this:

>  ...: Bad SafeTask: check for unsafe aliasing in arguments or return type;
>  also ensure your callable is stateless.
>
> note: in instantiation of member function
> '`folly::coro::detail::SafeTaskPromise<folly::safe_alias::co_cleanup_safe_ref,
> void, folly::coro::co_cleanup_capture<folly::coro::SafeAsyncScope<NeverCancel>
> &>, int>::get_return_object`' requested here
>
>   10 | `CoCleanupSafeTask<void> doOptionalWork(`

The error can be improved & made more granular, but if you skim
`SafeAliasAndClosure.md`, you'll know it signals a potential lifetime bug:
 - The coro's requested safety level is `co_cleanup_safe_ref`.
 - But you're passing a `co_cleanup_capture` of a **lower** `shared_cleanup`
   safety level i.e. a "shorter lifetime-arg into a longer-lifetime coro".

And indeed, the above code is trying to do something contradictory:
  - `optionalScope` should only be joined when `requiredScope`
    has finished joining.

    ⇒ `optionalScope` should outlive `requiredScope`
  - Since `bigwigInterest()` might get escalated via the `isRequired()` branch,
    it must be safe to add to `requiredScope` from an `optionalScope` task.

    ⇒ `requiredScope` should outlive `optionalScope`

You **cannot have it both ways** without additional synchronization! So, the
classic code above will compile, run, and suffer from a heisenbug that
occasionally hits the following assertion in `AsyncScope.h` -- but only during
last-minute `isRequired()` escalations:

```cpp
assert(
    !joined_ &&
    "It is invalid to add() more work after work has been joined");
```

Let's end with safe code that compiles **and** works:

```cpp
CoCleanupSafeTask<void> doWork(WorkItem)  { co_return; }
ClosureTask<void> doOptionalWork(
    co_cleanup_capture<SafeAsyncScope<CancelOnExitOrRequest>&> optionalScope,
    WorkItem item) {
  if (co_await item.isRequired()) { // Last-minute escalation?
    // We can't use `requiredScope` here since that might have already been
    // joined.  Instead, make the newly-required task non-cancelable on
    // `optionalScope`, which is kept alive by the current coro.  Note: It
    // would also be valid to plumb through the "parent closure" token here.
    optionalScope->with(co_await co_current_executor).schedule(
        co_withCancellation(CancellationToken{}, doWork(std::move(item))));
  } else {
    co_await doWork(std::move(item));
  }
}

co_await async_closure([](auto requiredScope, auto optionalScope)
        -> ClosureTask<void> {
      const auto& exec = co_await co_current_executor;
      requiredScope->with(exec).schedule(doWork(iAmMandatory()));
      requiredScope->with(exec).schedule(doWork(nonNegotiable()));
      optionalScope->with(exec).scheduleScopeClosure(
          doOptionalWork, bigwigInterest());
      optionalScope->with(exec).scheduleScopeClosure(
          doOptionalWork, canDoThisLater());
    },
    safeAsyncScope<NeverCancel>(), // Match "classic". Or: `CancelViaParent`
    safeAsyncScope<CancelOnExitOrRequest>());
}
```

You can verify this compiles by looking at the `SafeAsyncScopeTest.cpp` test
named `TwoScopes_Example_From_LifetimeSafety_md`.

## Safety benefits: Closing words

There are many code permutations that results in lifetime bugs. Some are similar
to the prior examples, some less so. Most bugs follow this simple plan:

> A task got moved so that its lifetime extends past its natural lexical scope.

More rarely, you will run into async cleanup ordering issues, such as in the
"Two async scopes" example above. And, there are other tricky possibilities not
covered above, like "passing short-lived references to long-lived scopes".

The great thing is that `folly::coro::safe` makes all of those kinds of bugs
dramatically harder to write!

Historically, the C++ safety philosophy was "you should know what you are
doing". If you study the examples above, a reasonable conclusion might be that
-- when deferred tasks and async scopes are involved -- we (humans) don't know
what we're doing:
  - Lexical scope rules don't fully protect us.
  - Bugs can lurk on rarely taken cleanup path or in race conditions, so unit
    tests don't save the day either.
  - Even if you're lucky enough to be using large-scale fuzz testing like
    Antithesis, it is still quite expensive to discover and fix heisenbugs like
    the "Two async scopes" example above.

The standing `folly::coro` safety recommendations resemble "know what you're
doing", but experience shows that's not enough to protect working programmers:
  - Use `co_invoke` when you need it,
  - Be very careful with pass by-reference, or `this` in member coros,
  - Avoid ephemeral lambda coros,
  - Don't start detached tasks,
  - Remember to join scopes and await `cleanup()`, in an exception-safe way,
  - Be careful with `co_scope_exit` lifetime, it runs longer than the task,
  - etc.

In contrast, `folly/coro/safe` idioms are materially safer, reduce the rules you
have to remember, and can be used routinely without runtime cost. As a result,
the effort of learning the safety wrappers amortizes well, once you default to
writing safer code.

The cost-benefit boils down to:
 - *Cost*: Adopt `SafeTask` & `capture` vocabulary & concepts, plus the
   occasional `FOLLY_INVOKE_*`".
 - *Benefit*: "Shift left": Convert runtime issues into compile errors -- both
   ASAN test failures, and production heisenbugs that takes weeks to track down.

The price of inaction is high and inevitable, so it pays to write safer code now.

If/when the [C++ lifetime safety profile P1179R1](https://wg21.link/P1179R1)
lands, and gets extended with async lifetime checks, your lifetime-safe code can
become shorter -- but its structure will stay the same (see
`RelationToWGProposals.md`). Meanwhile, in the intervening years, a bit of
`SafeTask` and `capture` boilerplate will provide P1179-aligned safeguards in
async code, where you need them most.

Lastly, here's a side benefit. Compared to `NowTask`, `SafeTask` offers more
flexibility to library authors -- the `async_closure()` variant that invokes
`scheduleUnsafe` **must** be awaited immediately, while the safer closure can be
moved around like a self-contained value. Of course, if closure inputs include
`capture` refs, its safety level is adjusted automatically (see
`SafeAliasAndClosure.md`). The `collectAllWindowedAsync` example would be would

Hopefully, the above advantages will convince you to give safer patterns a try!

---

## *Aside*: Should we add that `scheduleUnsafe()` after all?

Not the way it was shown in the above pseudo-code. As you saw in the "BAD"
examples above, restricting `schedule()` to take `CoCleanupSafeTask` detects
some very-hard-to-spot bugs.

That said, it *can* be useful to have an "I really, really know what I'm doing"
escape hatch. For example, an alternative correct implementation for the "Two
async scopes" example would be to:
  - Add a third `escalationScope` owned by an outer closure,
  - `scheduleScopeClosure` the inner closure on that scope,
  - Have `doOptionalWork` schedule escalated tasks on `escalationScope`.

This design is lifetime-correct, but `folly/coro/safe` does not currently
provide a generic way to query the `capture` of to the scope I'm running on. So,
to persuade the above solution to compile, we'd have to plumb the
`escalationScope` wrapper into `doOptionalWork` via a `manual_safe_val`.

*Aside*: `FutureWork.md` mentions a more general but elaborate solution --
providing `co_await co_cleanup_capture_of_owner` on a new `CoCleanupOwnedTask`.

In conclusion, start by giving the safer APIs a chance. Then, see if marking a
specific argument as `manual_safe_*` does the trick. If you truly have a
use-case that benefits from manually marking up a plain `Task` as a `SafeTask`
of a particular level, please:
  - Submit a patch to expose a factory method like
    ```cpp
    static SafeTask<Safety, T> manuallyAttestedAsSafe(Task<T>);
    ```
  - Insist on each usage of that function clearly explaining why it's safe.
