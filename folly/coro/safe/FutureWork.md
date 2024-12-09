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

### Linters!

This topic is so important that it has its own doc: `FutureLinters.md`.

### `SafeAsyncGenerator`

The largest feature gap of `coro/safe` is that it lacks extensions for
`AsyncGenerator`.  Integrating this would be a finite, well-defined project that
**should** be built, given an impactful use-case. Be sure to grep for
breadcrumbs in the code.

### *(easy)* Pass locals as `capture<>` refs into sub-closures

The use-case is pretty simple -- we want to create a new "child" lexical scope,
such that the parent's locals will outlive any `co_cleanup` associated with the
child. Since we want RAII-like behavior, there's no avoiding making a new
coroutine frame for the child, but at least there's [some hope](
https://clang.llvm.org/docs/AttributeReference.html#coro-await-elidable) of HALO
making it cheap. Sample usage:

```cpp
std::atomic_int n = 0;
co_await async_closure([](auto scope, auto n) -> ClosureTask<void> {
  using BgTask = CoCleanupSafeTask<void>;
  auto exec = co_await co_current_executor;
  scope.with(exec).schedule([](auto n) -> BgTask { n->fetch_add(100); });
  scope.with(exec).schedule([](auto n) -> BgTask { n->fetch_add(20); });
  scope.with(exec).schedule([](auto n) -> BgTask { n->fetch_add(3); });
}, safeAsyncScope<CancelViaParent>, capture_ref(n));
assert(123 == n.load());
```

The only new functionality above is that `capture_ref(n)` is now allowed in
`async_closure()` args, with the following effects:
  - Following normal `async_closure()` reference upgrade rules, the child
    closure gets a `capture<std::atomic_int&>`, or `&&` with
    `capture_ref(std::move())`, or a `after_cleanup_capture` if it also has a
    `shared_cleanup` arg.
  - There are two options for the safety of the resulting `async_closure()`:
    * Conservatively, it could just be `NowTask` and the references would be
      guaranteed valid.
    * For more user flexibility, it would be within the spirit of regular RAII
      to defer awaiting the closure to a later point in the current scope. That
      is, the `SafeTask` taking these `capture_ref()` args would be marked down
      to `<= lexical_scope_ref` safety. This is a new safety level with:
      ```cpp
      after_cleanup_ref >= lexical_scope_ref >= shared_cleanup
      ```
      In practice today, there's little difference from `after_cleanup_ref`,
      since `move_after_cleanup` won't take a `SafeTask` anyway. However,
      semantically, the valid lifetime for this body-only `SafeTask` is clearly
      shorter -- it expires whenever the captured refs expire, which (under
      typical RAII) is a bit longer than the lexical lifetime of the task. In
      this scenario, the user can, of course, invalidate the reference before
      awaiting the task, but it takes a bit of effort, and should be covered if
      the [P1179R1 lifetime safety profile](https://wg21.link/P1179R1) is
      standardized. For example:
      ```cpp
      std::optional<SafeTask<safe_alias::lexical_scope_ref, void>> t;
      {
        int i = 5;
        t = async_closure([](auto i) -> ClosureTask<void> {
          std::cout << *i << std::endl;
          co_return;
        }, capture_ref(i));
        ++i;
      }
      // BAD: The reference to `i` is now invalid!
      co_await std::move(*t);
      ```

*Note 1*: Initially, I wanted to hijack `folly::bindings::by_ref` instead of
`folly::coro::capture_ref` as the verb above, but realized this would cause
users to want to type `argName.` instead of `argName->`.

*Note 2*: The way that `async_closure(..., capture_ref(...))` acts, it seems
like we could just universally allow creating `lexical_scope_capture<T&>` from
`T&`. `Captures.h` would need to support auto-upgrade of `lexical_scope_capture`
to `capture` or `after_cleanup_capture`, depending on `shared_cleanup` status.
This "universal" implementation would be more complex, and I would start by only
building the above special case of `async_closure(..., capture_ref(...))`. The
reasons is that, without `async_closure()`'s capture-upgrade semantics, there's
not a lot of value in having a `lexical_scope_capture<Ref>` -- you can't use it
to schedule work on a nested `SafeAsyncScope`. Whereas for immediately-awaited
tasks, `NowTask` + pass-by-reference is much simpler.

### *(lo-pri)* `co_cleanup_capture<>` from `AsyncObjectPtr<>`

Before considering this item, first implement the preceding "create `capture<>`s
via `capture_ref()`" idea. That enables passing `capture_ref(*objPtr)` into a
sub-closure, which is most of what you need.

The utility of giving the sub-closure a `capture` is that `AsyncObjectPtr<>` is
move-only, whereas `capture` lets you pass references into further descendants.

In some scenarios, the `capture_ref()` solution may be less convenient, since it
requires you to independently keep the object alive.

Today, you can do this kludge to move the `AsyncObjectPtr` into the closure
**and** get a `capture` ref. However, it's ugly and fragile:

```cpp
auto& rawObj = *objPtr;
co_await async_closure(..., capture_ref(rawObj), std::move(objPtr));
```

The better fix would be to special-case moving `AsyncObjectPtr` into a closure,
subject to 2 requirements:
  - The closure becomes the owner of the `AsyncObjectPtr`, i.e. its cleanup
    posts the cleanup baton for the object. *Future*: It would also be neat to
    think about reattaching the object's `co_cleanup` to the closure, but this
    operation changes semantics, and probably should be explicit & separate (not
    to mention that it seems hard to implement with the current
    `folly::AsyncScope`).
  - The inner task of the closure gets a `co_cleanup_capture<Obj&>`. *Future*:
    There might be a case for providing a nullable reference kind, too, but it's
    hard to justify that extra vocabulary when the kludge above kind of works.

### *(easy)* TBD: Allow by-value `this` variables?

In `LifetimeSafetyBenefits.md`, this example comes up:

```cpp
tasks.push_back(async_closure(
    FOLLY_INVOKE_MEMBER(bar), as_capture(Foo{index})));
```

At first glance, it looks like we could elide the `as_capture` for `this`:

```cpp
tasks.push_back(async_closure(FOLLY_INVOKE_MEMBER(bar), Foo{index}));
```

Are there any bad second-order consequences, or should we make this simple
improvement, and update the other guide?

### `co_await co_cleanup_capture_of_owner`

Check out the `LifetimeSafetyBenefits.md` discussion titled "Should we add
`scheduleUnsafe()` after all?". That benefits from a more generic approach to
recursive scheduling of background tasks:
  - A new `CoCleanupOwnedTask` type that stores a ref to its `co_cleanup` owner.
    Owners are expected to implement async-scope-like lifetime semantics: the
    reference is valid while the task is alive.
  - This task can get a `co_cleanup_capture<>` wrapper via

    ```cpp
    auto owner = co_await co_cleanup_capture_of_owner;
    ```
  - `SafeAsyncScope`'s scheduling primitives are updated with a protocol to set
    the owner.

This would largely supersede the `scheduleScopeClosure` / `scheduleSelfClosure`
APIs, but would not replace `CoCleanupSafeTask`, which -- being lighter -- would
remain the task of choice for non-recursively-scheduling tasks.

### `co_setup` support

We already have two-phase destruction, with `async_closure()` and `AsyncObject`
invoking `co_cleanup()` before destroying the object. This is *essential*, for
example, in order to correctly join `SafeAsyncScope`.

It would be possible, but less critical, to introduce an analogous two-phase
*construction* protocol, where `async_closure()` and `AsyncObject` always await
this task for each `capture` / `Slot` that implements it:

```cpp
Task<void> co_setup(async_closure_private_t);
```

Reasonable semantics would be:
  - `co_setup()` is awaited after all `setParentCancelToken()` calls finish.
  - Once `co_setup()` is invoked successfully for an object, its `co_cleanup()`
    counterpart is guaranteed. It's TBD if we want to guarantee the converse --
    that `co_cleanup()` is not invoked if `co_setup()` has not yet been invoked.
    The simplest implementation wouldn't make the second guarantee.
  - It's allowed to have `co_cleanup()` without `co_setup()` and vice-versa.
  - Like `co_cleanup()`, the presence of any `co_setup()` forces an outer coro,
    since that's the simplest implementation.

Support for automatic `co_setup()` is less critical because there are decent
workarounds:
  - An object needing to await setup can explicitly require that from the user
    using the object. Unlike async cleanup, forgetting to perform manual
    two-phase setup is guaranteed to be caught by even the most basic of tests.
  - For objects that don't require async cleanup immediately after setup, it's
    also reasonable to wrap the cleanup closure with another coro that awaits
    setup for some objects, before moving them into the closure.

Despite the workarounds, the `co_setup` / `co_cleanup` duality is simple to
understand, and to support, and so would make a good addition to the library.

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
    safeAsyncScope<CancelViaParent>(),
    // Like in Python, kwargs must follow positional args, and are collected
    // into a single `auto kw` above.
    "a"_id = as_capture(make_in_place<std::atomic_int>(3)),
    "b"_id = 3);
```

### Better non-block scope emulation

The current `async_closure()` is like a block scope -- as in old-school C, one
has to pre-declare all the block's variables at the start of the closure. This
can be quite inconvenient when you can only initialize a certain capture mid-way
through a closure scope.

There are two common reasons to have such a capture -- either have the closure
trigger `co_cleanup` for it, or to reference it in a `co_cleanup_capture`. Both
only apply when the closure has an outer coro. In both cases:
  - The closure has to handle `co_cleanup` and destruction.
  - So, it needs to own the memory, and I don't see a way to avoid
    pre-declaring it in the `async_closure()`'s argument list.

Nonetheless, delaying initialization can be valuable.

As noted in "Emulating non-block scopes" in `AsyncClosure.md`, using
`capture_indirect` with an nullable generic type gets us partway to a usable
solution, but two problems remain:
  - No support for `co_cleanup` captures.
  - No good way to get a non-nullable `capture` ref after initialization, even
    though "initialize once, and use" is the most reasonable pattern.

It would be quite messy to make both of those work with `capture_indirect`, so I
propose a "dual" mechanism along these lines:
  - A closure argument is declared as `delayed_init(as_capture(...))`.
  - The template wrapper `delayed_init<>` is a dressed-up `std::optional`,
    something like:
    ```cpp
    template <typename CaptureT>
    class delayed_init {
     public:
      // rvalue-qualified so use-after-move lint can enforce "init once"
      auto init(auto fn) && {
        if (FOLLY_UNLIKELY(t_)) {
          LOG(DFATAL) << "Already initialized";
        } else {
          t_.emplace(fn());
        }
        constexpr bool has_shared_cleanup =
            /* true iff `CaptureT` is an `after_cleanup_` type */;
        return t_->to_capture_ref<has_shared_cleanup>(
            capture_private_t{});
      }
      // TODO: pass-throughs for `co_cleanup()` and `setParentCancelToken()`.
     private:
      std::optional<CaptureT> t_;
    };
    ```

The net effect is that:
  - We can delay the initialization of **any** kind of `capture`.
  - Initialization is (almost) statically guaranteed to happen only once.
  - Once initialized, you have a regular, non-nullable `capture` ref.

Some implementation concerns include:
  - Making this wrapper commute with all the `capture` binding logic.
  - With a `co_setup()`-capable class, `init` should probably be constrained
    away, and `co_init()` provided instead.

This would be best coupled with an additional linter that prevents moving
`delayed_init` values out of the original closure scope.

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
of its owned `capture`s to `after_cleanup_ref`. This feels like a UX wart.
"Future work" above proposes `restricted_co_cleanup_capture` to somewhat improve
the UX. **That** is my best zero-cost idea.

Unfortunately, fully removing this restriction is at least as hard as the next
paragraph on "allowing `capture<>` refs to be returned" -- which is shown to be
untenable. To see why, imagine the child closure scheduling a task on the
ancestor's async scope. This has exactly the same lifetime consequences as
returning a reference.

### Let `async_closure()` / `SafeTask` return `capture<>` refs

Two directions were considered:

  - The "dream" would be for each reference to know the outer-most scope, in
    which it is valid, and refuse to be returned outside of it. C++ allows
    recursion, so there is no way to statically determine this in all cases.
    Whereas, explicit runtime checks (like depth-counting references) wouldn't
    be much better than ASAN -- just a minor usability boost.

  - A limited version of the "dream" would involve tagging a `capture<>` with a
    list of automatic type for each scope that it *might* be valid in. This
    would catch non-recursive memory safety bugs, but would come with a hefty
    compile-time and usability cost -- `coro/safe` errors are already kind of
    long. So, didn't seem to be worth it.

### OK, no returning plain refs. Can we extend lifetimes? Maybe...

What if a `capture<>` could extend its own lifetime so that it can be passed
around like a value type?

Good news -- we have `as_capture_indirect()` sugar to make it ergonomic to
capture smart pointers. So you can already use:
  - `as_capture_indirect(std::make_unique<T>(...))`,
  - `as_capture_indirect(std::make_shared<T>(...))`,
  - where thread-safety is not a concern, a nonatomic-refcounted shared pointer
    to avoid cache synchronization costs.

However, none of those are adequate to extend the lifetime of
`co_cleanup_capture`s, which would additionally require their cleanup task to be
reparented to a longer-lived "async scope". This is certainly possible to implement, but would take more than minor surgery.

Extending `co_cleanup_capture` lifetime implies the extending the lifetime of
the closure's entire cleanup tuple. That's because `co_cleanup_capture`s (e.g.
`SafeAsyncScope`) can currently contain references to non-cleanup `capture`s
from the current closure, or to any `capture`s from ancestor closures.
Therefore, the type passed to the parent closure will need to do all of these:
  - Either contain a type-erased tuple storage & destructor, or expose a very
    complex signature.
  - Either delegate cleanup to a `parentScope` (and this should support
    `restricted_co_cleanup_capture`), or move the lifetime-extended thing into a
    pre-prepared placeholder slot provided by the parent closure.
  - Either ensure ownership is being transferred to the closure's direct parent,
    or trigger the automatic lifetime extension of the entire chain of
    intermediate ancestors -- the thing being extended could reference any of
    them. In the latter case, keep in mind that
    `least_common_ancestor(PrevOwner, NextOwner)` may be a third entity.
  - Consider thread-safety. Transferring ownership to the direct parent should
    be safe, but more complex patterns may need synchronization.

It should be possible to implement this well, and it would be more general than
today's two *partial* solutions to this problem:
  - `after_cleanup_` safety downgrade due to `co_cleanup_capture` refs having
    `shared_cleanup` safety.
  - `restricted_co_cleanup_capture` references that can only ingest
    `maybe_value` objects.

Unfortunately, by comparison, building the lifetime extension logic is tricky!
And so is giving it a nice user-facing API. For example, one might:
  - Define a special "lifetime extender" type that can only be passed from a
    child closure to the immediate parent, or
  - A lifetime extension type could be `co_return`ed, in which case it it would
    also take some figuring out to make this protocol compose nicely with "chain
    some work after cleanup". A particularly likely "bad outcome" is if
    `move_after_cleanup` destructively mutates something that still has a live
    reference.

So, until there's strong evidence that users need it, there's no intention to
allow changing ownership of `co_cleanup_capture`s.
