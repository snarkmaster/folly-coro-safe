## Purpose & audience

You're seeking to deeply understand, enhance, or verify the lifetime safety
machinery of `folly/coro/safe`.

Before starting, get familiar with `LifetimeSafetyBenefits.md`, and the docs
it references.

## How does lifetime safety work?

### Lifetime safety rule system

This outline is meant to help understand how `SafeTask`, `async_closure`, and
`capture` details fit together to enforce lifetime safety. The code and related
docs contain more detail & design notes.

The explanation is organized *hierarchically*, so you might first read the
high-level bullets, and then drill into the details.

  - Below, "safety" refers to `safe_alias_of_v` of a type. These levels are
    always relative to the current coro, and are estimated conservatively.
    Details in `SafeAliasAndClosure.md`.
      * Safety is compared via `>=`, greater safety meaning "longer lifetime".
      * For example, a type with `after_cleanup_ref` safety is valid:
          - In the body of the current coro,
          - If it's an `async_closure`, during `result_after_cleanup()`,
          - **NOT** valid in `co_cleanup` tasks (somewhat paradoxically).

        For validity in all 3 contexts, you need a type with higher safety, e.g.
        `co_cleanup_safe_ref` or `maybe_value`.

  - There is a family of `capture` types, like `co_cleanup_capture`,
    `after_cleanup_capture`, etc. This dereferenceable type wrapper also tracks
    value category. The type name records the lifetime safety of its data, and
    enforces associated access safety constraints. Details in `Captures.md`.
    - `capture<Val>` is never moved or copied from the owning closure.
      - They're non-copiable.
      - They use `safe_alias_of_v` checks (and a future linter) to prevent the
        wrapper from being moved. You can move the contained value. Another
        future linter will ensure that the use-after-move linter applies here.
    - `capture<Ref>`s can be passed (copied or moved) to child coros.

  - Users needing a movable task should write either:
      * A bare `SafeTask` coro, or
      * A "safe closure", defined as an `async_closure` with an inner `SafeTask`
        (usually `ClosureTask`) and inputs `>= closure_min_arg_safety`.
          - If these conditions are met, `async_closure` returns a `SafeTask`
            marked with the safety of its least-safe argument.
          - If the conditions are not met, it emits a `NowTask`.

  - `SafeTask` & safe closure safety is constrained by the safety of its inputs:
      * Both prohibit "stateful callables", like lambdas with captures, bound
        member functions, or other non-empty classes with `operator()`. This is
        required because we wouldn't be able to measure the safety of those
        inputs.
      * Both prohibit passing raw references, raw pointers, and various other
        **known** instances of unsafe aliasing (subject to the limitations in
        `SafeAlias.h`, to be improved with C++26 reflection
        [P2996](https://wg21.link/P2996)).
      * Both may only return types of `>= maybe_value` safety, since the other
        levels are not guaranteed to be valid outside of the current coro.
          - In particular, `capture<Ref>` may **not** be returned.
          - See `SafeAlias.h` for the "callable hole", which can only be fixed
            with stronger reflection.
      * The implicit object parameter (a reference) is exclusively permitted in
        the non-movable `MemberTask`.
          - Used alone, this `SafeTask` is like a restrictive `NowTask`.
          - Used in `async_closure`, it is upgraded to a movable `SafeTask` if
            the arguments' safety allows.
      * The safety of the resulting `SafeTask` is guaranteed to be `>=` that of
        the arguments. With `async_closure`, you get `==`.

  - Due to the above restrictions, users can only pass references into
    `SafeTask`s via `capture<Ref>`s.
      * Owning `capture<Val>` wrappers are **only** created via `async_closure`
        and `AsyncObject`. They share much of the implementation.
      * Today†, `capture<Ref>` can **only** be obtained from a
        `capture<Val>`, either via the implicit conversion operator in
        `Captures.h`, or via `async_closure` argument binding logic.
          - `async_closure` knows that an argument `capture<Ref>` is going to a
            child coro, and therefore can *usually* "upgrade" its safety --
            greatly improving usability. Details in `Captures.md`.
          - In contrast, a bare `SafeTask` taking an `after_cleanup_capture`
            from the parent will get a `after_cleanup_capture`, which is often
            an underestiamte of the ref's safety relative to the child. The
            reason for this deficiency is that the C++ coroutine specification
            lacks a facility for rewriting argument bindings.

        > † `FutureWork.md` proposes a new `lexical_scope_ref` safety level to
        represent regular C++ lexical scope safety, making it weaker than even
        `after_cleanup_ref`. This is relevant because it could be directly
        obtainable from scope variables -- but even if implemented, this would
        not invalidate the conclusion below.
      * **Conclusion:** Together with the return-value safety restriction, these
        rules guarantee that a `capture<Ref>` always has a *conservative*
        measurement of the lifetime safety of its data.

### Recap of lifetime safety rules

Here are the key takeaways from the above rule system. Subject to various
caveats (due to very limited reflection in C++20), we know that:
  - A `capture<Ref>` is always valid, because it can only point to data owned
    by a parent on its call / `co_await` stack.

    Note that `AsyncObject`s can only take `>= co_cleanup_safe_ref` args,
    meaning their lifetime safety is just like that of scope-schedulable
    closures. And yes, an `AsyncObject` has a "parent" just like a closure:
      - The lexical scope that made it owns the reference, an `AsyncObjectPtr`.
      - A `SafeAsyncScope` must also be provided at construction time, taking
        responsibility for the object's cleanup & destruction. It is strictly
        longer-lived than the lexical scope.
      - **Conclusion:** Any references taken by an object are `>=
        co_cleanup_safe_ref` in its originating lexical scope, so they are safe
        to use in tasks scheduled on the async scope, or its descendants.
  - Its safety level (`safe_alias_of_v`) conservatively describes its lifetime.
  - The safety of a `SafeTask` (including safe closures) incorporates the safety
    of all its inputs.
  - Thus, both `SafeTask`s and `capture`s are both safe to pass around in this
    ecosystem, and the measured safety level remains conservatively correct.

### Lifetime safety rules in action

Let's analyze this example in terms of safety:

```cpp
assert(123 == co_await async_closure([](auto scope, auto n) {
  *n += 20;
  scope->with(co_await co_current_executor).schedule(
      [](auto n) -> CoCleanupSafeTask<void> {
        *n += 3;
        co_return;
      });
  co_return move_after_cleanup(n);
}, safeAsyncScope<CancelViaParent>(), as_capture(100));
```

The high-level summary is that, in order to pass references into an async scope
task, you must put the data `as_capture` on the closure that owns the scope, or
its parent‡. Here's how that works in detail.

> ‡ Once `capture_ref` is built (`FutureWork.md`) it will also be possible to have
non-`capture` locals from parent coros become safe captures in the child.

To run a task on `scope`, we want to be sure the task's inputs will outlive the
scope's `join()`. We further know that:
  - After the inner coro of an `async_closure` exits, it will:
      * First, await `co_cleanup()` for any owned captures (`scope`).
      * Then, once cleanup completes, destroy non-cleanup owned captures (`n`).
  - The safety levels for this closure's owned captures are:
      * `shared_cleanup` for `scope`, since the wrapped type has `co_cleanup()`.
      * `co_cleanup_safe_ref` for non-cleanup `n`. *Edge case*: if the closure
        took a non-owned argument with `shared_cleanup` safety, then `n` would
        only get `after_cleanup_ref` safety.

        *Rationale*: Non-cleanup `capture`s are destroyed after `co_cleanup()`s
        complete, so it is safe for cleanup tasks to reference them. But, if we
        have a ref to a parent's `co_cleanup` task, then we'd better not pass it any of our own non-cleanup `capture`s.
  - `SafeAsyncScope`'s `schedule()` method requires a `SafeTask` with safety
    `>= co_cleanup_safe_ref`. This means:
      * We can pass `n` to the task scheduled on `scope`.
      * We **cannot** pass `scope` to any scope's `schedule`, Since
        `shared_cleanup` < `co_cleanup_safe_ref`.

        This is correct for "other" scopes -- the "Two scope example" above
        showcases why. To support recursive scheduling on the **same** scope, we
        provide `scheduleScopeClosure()` that safely passes a same-scope
        reference as the first argument of the `async_closure` it creates.
  - `move_after_cleanup` requires safety `>= after_cleanup_ref`, and `n`
    satisfies this.

A couple of other examples that **would not** compile in the above closure:

  - Since `SafeTask` forbids pass-by-reference:
    ```cpp
    int m = 3;
    scope->with(co_await co_current_executor).schedule(
        [](auto n, int& m) -> CoCleanupSafeTask<void> {
          *n += m;
        });
    ```
  - Since `move_after_cleanup` checks safety, and `scope` has safety
    `shared_cleanup < after_cleanup_ref`.
    ```cpp
    co_return move_after_cleanup(scope);
    ```
