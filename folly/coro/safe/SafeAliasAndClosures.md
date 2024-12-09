## Audience & purpose

You're familiar with the high-level concept of `SafeAlias.h`, and have read
through `AsyncClosure.md`. You understand that `SafeTask` type aliases like
`ValueTask`, `CoCleanupSafeTask`, `MemberTask`, `ClosureTask` are just syntax
sugar, and correspond 1:1 to `safe_alias` levels.

This doc will help you deeply understand the closure-specific hierarchy of
`safe_alias` values, and to see how these correspond to `async_closure()`'s
lifecycle.

XXX Update with `async_now_task`, `LifetimeSafety.md`

## Demo of reference lifetimes and `async_closure()`

In the slightly contrived example below, we have three nested task invocations.
For short, let's call them `A` invoking `B` invoking `C`.

```cpp
assert(42 == co_await async_closure(
  [](auto scope1, auto n1)
      -> ClosureTask<move_after_cleanup_t<std::atomic_int, int>> { // A
    assert(2 == co_await async_closure([](auto scope2, auto n2, auto toAdd)
        -> ClosureTask<move_after_cleanup_t<int>> { // B
      scope2->with(co_await co_current_executor).schedule( // C
          [](auto n3, int k) -> CoCleanupSafeTask<void> {
            n3->fetch_add(40);
          }(n2, k));
      n2->fetch_add(*toAdd);
      co_return move_after_cleanup(toAdd);
    }, scope1, n1, as_capture(2)));
    co_return move_after_cleanup_as<int>(n1);
  },
  safeAsyncScope<CancelViaParent>(),
  as_capture(make_in_place<std::atomic_int>(0))));
```

There are actually 4 coroutines here. The `scope` argument of `A` requires async
cleanup (`SafeAsyncScope` has `co_cleanup()`), so there's an `outerA` which
manages the cleanup -- akin to a `try-catch` block, and an `innerA` which is the
function body you see above. In contrast, for `B`, no "outer task" is created,
since no cleanup is required.

If you want to read the implementation as you follow along, the pseudocode in
`AsyncClosure.md` is far easier to read than the real `detail/AsyncClosure.h`.

Here are the `safe_alias` levels in relation to this snippet, from strongest to weakest:

  - **`maybe_value`**: `int k` in `C`. This is the normal pass-by-value
    semantic that is universally safe. Other examples would be passing
    `std::string` or `std::unique_ptr`.

  - **`co_cleanup_safe_ref`**: `n1`, `n2`, and `n3`, all
    `capture<std::atomic_int&>`. This safety level may be passed into
    `CoCleanupSafeTask` (aka `SafeTask<co_cleanup_safe_ref>`), which is the
    least-safe kind of task that can be scheduled on `SafeAsyncScope`. The level
    `co_cleanup_safe_ref` is also the minimum safety for `AsyncObject`
    constructor arguments, since those are expected to contain at least one
    `Slot` requiring `co_cleanup()`.

  - **`after_cleanup_ref`**: `toAdd` of type `after_cleanup_capture<int>`.
    This is the lowest safety level for a capture that can be returned via
    `move_after_cleanup`. Such captures are valid both within the closure's
    body, and until after it finishes awaiting its `co_cleanup()` tasks.

  - **`shared_cleanup`**: `scope1` and `scope2`, both
    `co_cleanup_capture<SafeAsyncScope<CancelViaParent>>`. This is the
    least-safe kind of capture that can be passed to an `async_closure()` and
    still make a `SafeTask`. When a coro argument has this safety level, that
    implies the referenced `co_cleanup` capture came from an ancestor of the
    coro, and the coro **must not** pass anything with its own lifetime (eg
    owned captures), or shorter, into such `co_cleanup` args. The practical
    implementation of this "must not" rule is the reference upgrade/downgrade
    logic mentioned below.

The `safe_alias` levels prefixed with `unsafe` are not currently allowed in
`async_closure()` arguments (`AsyncObject` requires `>= co_cleanup_safe_ref`,
per above). To permit unsafe args, `async_closure` would need to support
returning a non-movable task like `NowTask`. You can do this today with generic
coros that are not closures -- check out `AutoSafeTask`, which resolves to
either `SafeTask` (for levels >= `shared_cleanup`) or `NowTask`.

  - For **`unsafe_closure_internal`** and **`unsafe_member_internal`**, you will find a
    detailed discussion of their usage in `SafeTask.h`. We need these
    sort-of-`SafeTask`s because its `coroutine_traits`-based implementation is
    the only way to ascertain that a coro doesn't take any arguments by
    reference. So, the point of these types is to signal the lack of pass-by-ref
    to `async_closure()`, which will proceed to safety-check the arguments, and
    output a `SafeTask` with the correct safety level instead.

  - **`unsafe`** means any detectable aliasing (raw pointer, reference,
    reference wrapper, etc) not automatically proved to be safe via the
    `capture` logic, or manually claimed to be safe via a `manual_safe_*`
    wrapper. Today's C++ lacks reflection, so the detection heuristic has
    limitations -- see `SafeAlias.h`. This level cannot be used with `SafeTask`.

The full execution sequence is:
  - Create capture storage for `A`:
    ```cpp
    std::unique_ptr<std::tuple<
        co_cleanup_capture<SafeAsyncScope>, capture<std::atomit_int>>> storage;
    ```
    *Minor implementation detail*: This allocation also stores an empty
    `exception_ptr` whose pointer is passed into `co_cleanup()`.
  - Create `innerA`, binding its args to `capture` references:
      * `co_cleanup_capture<SafeAsyncScope<CancelViaParent>&> scope1` from
        `std::get<0>(*storage)`.
      * `capture<std::atomic_int&> n1` from `std::get<1>(*storage)`.
  - Create a `tuple<Task<void>> cleanupTasks` from `scope.co_cleanup()`.
  - Create `outerA`, moving `innerA`, `storage`, and `cleanupTasks` into it.

    Its type is `ValueTask<int>` aka `SafeTask<safe_alias::maybe_value, int>`, since:
      * Safety: The closure has no non-stored arguments.
      * Return type: `move_after_cleanup_t<..., int>` means the closure returns `int`.
  - Await `outerA`, entering its lambda body above.
      - Call `setParentCancelToken` on `scope1`.
      - Await `innerA`:
        - Create `B`, binding `scope2` and `n2` to copies of the `capture`
          references above.

          The coro's type is `SafeTask<safe_alias::shared_cleanup, int>` since
          it gets a reference to a `co_cleanup` argument from the parent
          (`scope2`). That makes it unsafe to pass `B`'s own captures (`toAdd`)
          into its `co_cleanup` args. To achieve this, `B` **downgrades** its
          own captures to `after_cleanup_ref` safety. Whereas, absent
          `shared_cleanup`, owned non-cleanup captures would have safety
          `co_cleanup_safe_ref`. By analogous logic, non-`shared_cleanup`
          closures actually **upgrade** `after_cleanup_capture<T&>` refs from
          the parent to plain `capture<T&>` refs -- and `B` lacks this upgrade.

          Its sole `as_capture()` arg doesn't need `co_cleanup()`, so there
          is no `outerB`.  Therefore, `after_cleanup_capture<int> toAdd` is
          moved into `B`.
        - Await `B`:
            - Create `C`, binding a copy of the `capture<std::atomic_int&>` to `n3`.
            - Schedule `C` in the background on `scope2`.
            - Without synchronizing with `C`, increment `n2` by `toAdd`. Thus,
              the atomic's value might either be `2` or `42` at this point.
            - The normal use-case of `move_after_cleanup` is in coros with
              cleanup, where it moves out value from the owned `capture`
              **after** the outer task successfully awaits its `cleanupTasks`.

              The safety level `safe_alias::after_cleanup_ref` is named after
              this feature. Such references outlive the closure's body and
              `co_cleanup` phase, and so are fine to use with
              `move_after_cleanup` or similar -- but they are **unsafe** for use
              with `co_cleanup` args in the current closure.

              Here, `move_after_cleanup` sees that `toAdd` is a value (not
              reference) `capture<int>`, meaning the current closure lacks
              cleanup.  So, it shortcuts to moving out the contained `int`
              -- this is done to keep "after cleanup" and "outer coro" as
              independent aspects of the UX.
        - The last thing `innerA` does is `co_return
          move_after_cleanup<int>(n1)`. This just copies the `std::atomic_int&`
          into a special class to be trasformed in `outerA`.
      - Await each of `cleanupTasks`, whether or not any of the prior code threw.
        - This will join `scope1`, thereby awaiting `C`. At this point, `n1`
          definitely points at `42`.
      - If `innerA` or `cleanupTasks` threw, propagate that error.

        Otherwise, on success, we notice that `innerA`'s return type of
        `ClosureTask<move_after_cleanup_t<>>` speaks the closure-internal
        `result_after_cleanup()` protocol. So, `outerA` invokes that to convert
        the `std::atomic_int&` to `int`, and `co_return`s it.
