## Future work & contributions

Please discuss your contribution with Github user @snarkmaster. When
extending `coro/safe`, it is important to maintain some core invariants, and
this can require careful thought and code review. Some specific points:

  - We want to guarantee that any type needing an async destructor can
    **only** be created in a managed setting (i.e. `as_capture` or
    as an `AsyncObject::Slot`). Failing to enforce this constraint would
    break our core promise of "guaranteed async cleanup".

  - It requires attentive analysis of a use-case to define effective
    `SafeAlias.h` markings and `capture`-like pointers for new scenarios
    like `AsyncGenerator`.  For example -- arguments passed **into** a
    "safe" variant of generator should follow `async_closure` rules.  But,
    we'll need to think carefully about yielding references **from**
    generators, and having `co_yield` references return a reference **into**
    the generator (supported in Python, but not yet in `folly::coro`).

With that said, here are the things that we want to add.

### `SafeAsyncGenerator`

The largest feature gap of `coro/safe` is that it lacks extensions for
`AsyncGenerator`.  Integrating this would be a finite, well-defined project that
**should** be built, given an impactful use-case. Be sure to grep for
breadcrumbs in the code.

### `async_named_closure`

Since `async_closure()` deliberately disallows lambda captures, it has a
usability gap when passing many arguments by value.  Luckily, this is easy to
resolve by adding a keyword argument syntax.  I have some WIP diffs for this,
but it would take some focused work to really polish it off:

```cpp
co_await async_named_closure(
    [](auto scope, auto kw) {
      scope.with(co_await co_current_executor).schedule(async_closure(
          [](auto kw2) {
            kw2["a"_id].fetch_add(kw["b"_id]);
            co_return;
          },
          kw));
      // NB: When adding `after_cleanup`, consider `co_after_cleanup` too.
      // Both would have `async_closure` argument semantics ...  and at
      // least the latter could perhaps even own its own `capture`s.
      // Also, the ideal syntax for this might be `.then()` or `operator|`
      // chaining of closure-like gadgets, with the `co_return` or `return`
      // of one being plumbed into the arguments of the next.
      co_return after_cleanup(
          [](auto kw2) { return kw2["a"_id].load(); },
          kw);
    },
    safeAsyncScope<CancelViaClosure>(),
    // Like in Python, kwargs must follow positional args, and are collected
    // into a single `auto kw` above.
    "a"_id = as_capture(make_in_place<std::atomic_int>(3)),
    "b"_id = 3);
```

### `restricted_co_cleanup_capture`

Your will find mentions of "restricted" `capture`s throughout the codebase.
That's an already-designed & simple-to-implement feature meant to address a
particular quirk of the `capture` safety system.  Specifically, today, when you
pass `async_arc_cleanup<T&>` into a closure, this forces the data owned by the
closure to have weaker memory-safety markings.  That prevents the inner closure
from passing a reference to something it owns into a task scheduled on the outer
closure's scope (or other "cleanup" arg).  If you find yourself inconvenienced
by this quirk, it is likely that implementing restricted refs can help -- a
closure is unaffected when a restricted ref is passed into it, and the
restricted refs can still be used to schedule `ValueTask`s on the corresponding
scope / `co_cleanup` object.

## Future non-work

This documents ideas that were contemplated as part of the `folly/coro/safe`
design, and did not make the cut. The most common cause for rejection was
"non-zero runtime cost" and "late / runtime diagnosis". The reality is that, for
runtime checks, ASAN & friends are extremely good, so if you are not testing
your project under sanitizers, **that** is the low-hanging fruit for improving
the safety of your code, and the custom runtime-safety measures below
would have worse ROI.

It _is_ possible to later revisit to some of these ideas, but the runtime cost would
have to be so low that one could plausibly leave the checks enabled in non-debug
builds. Otherwise, just stick with sanitizers.

### Lifting restrictions on how "async scope" refs are passed to sub-closures

Today, passing a `co_cleanup()` argument to a sub-closure downgrades the safety
of its owned `capture`s to "body-only". This feels like a UX wart. "Future work"
above proposes `restricted_co_cleanup_capture` to somewhat improve the UX.
That is my best zero-cost idea.

Unfortunately, fully removing this restriction would equvalent to the next
paragraph on "allowing `capture<>` refs to be returned" -- which is shown to be
untenable. To see why, imagine the child closure scheduling a task on the
ancestor's async scope. This has exactly the same lifetime consequences as
returning a reference.

### Let `async_closure()` / `SafeTask` return `capture<>` refs

Two directions were considered:

  - The "dream" would be for each reference to know the outer-most scope, in
    which it is valid, and refuse to be returned outside of it. C++ allows
    recursion, so there is no way to statically determine this in all cases.
    Whereas, explicit runtime checks wouldn't be much better than ASAN -- just a
    minor usability boost.

  - A limited version of the "dream" would involve tagging a `capture<>` with a
    list of automatic type for each scope that it *might* be valid in. This
    would catch non-recursive memory safety bugs, but would come with a hefty
    compile-time and usability cost -- `coro/safe` errors are already kind of
    long. So, didn't seem to be worth it.

### OK, no returning plain refs. Can we extend lifetimes? Maybe...

What if a `capture<>` could extend its own lifetime so that it can be passed
around like a value type?

Good news -- we already have `as_capture_unique()` sugar to make unique
ownership more ergonomic. It could be in scope to add similar `capture_shared`
syntax sugar, or even a `capture_nonatomic_refcounted` to avoid paying for
atomics.

However, none of those are adequate to extend the lifetime of
`co_cleanup_capture`s, which would additionally require their cleanup task to be
reparented to a longer-lived "async scope". This is certainly possible to implement, but would take more than minor surgery.

Extending `co_cleanup_capture` lifetime implies the extending the lifetime of
the closure's entire cleanup tuple. That's because `co_cleanup_capture`s (e.g.
`SafeAsyncScope`) can currently contain references to non-cleanup `capture`s
from the current closure, or to any `capture`s from ancestor closures.
Therefore, the type passed to the parent closure will need to do all of these:
  - Either contain a type-erased tuple storage & destructor, or have a very
    complex signature.
  - Either delegate cleanup to a `parentScope` (and this should support
    `restricted_co_cleanup_capture`), or move the lifetime-extended thing into a
    pre-prepared placeholder slot provdied by the parent closure.
  - Either ensure it's only being moved to the closure's direct parent, or
    trigger the automatic lifetime extension of the entire chain of intermediate
    ancestors -- the thing being extended could reference any of them.

It is certainly possible to implement this, and it would be more general than
today's two *partial* solutions to this problem:
  - `body_only_` safety downgrade due to `co_cleanup_capture` refs having
    `shared_cleanup` safety.
  - `restricted_co_cleanup_capture` references that can only ingest
    `maybe_value` objects.

But, by comparison, building the lifetime extension approach is tricky! And so
is giving it a nice user-facing API. For example, one might:
  - Define a special "lifetime extender" type that can only be passed from a
    child closure to the immediate parent, or
  - A lifetime extension type could be `co_return`ed, in which case it it would
    also take some figuring out to make this protocol compose nicely with "chain
    some work after cleanup".
