## Audience / purpose

You are working on a custom class that requires some cleanup operations to be `co_await`ed, before it gets destructed (unless it was just-constructed, see
"caveat" below).  You have a compelling reason **not** to use `SafeAsyncScope` or
`AsyncScopeObject` instead.  You have already reviewed `SaferCoro.md`, and
`AsyncClosure.md` or `AsyncObject.md`.

## How to implement `co_cleanup()` guaranteed async cleanup

### Can you *avoid* implementing it?

Often, `AsyncScopeObject` or `AsyncObject` will do what you need, while offering
a `Slot` API that is harder to misuse.

### If implement you must, then...

Types passed to async closures via `as_capture()` may define one of:

```cpp
co_cleanup(async_closure_private_t)
co_cleanup(async_closure_private_t, const exception_wrapper*)
```

The second variant can inspect the exception thrown by your inner coro, or
by the `setParentCancelToken()` callback on your arguments, if it exists &
throws. The pointer is never null, but should only be dereferenced once the cleanup task is being awaited. If, at that point, the `exception_wrapper` is empty, then the closure exited successfully.

`co_cleanup` tasks may throw, but it is discouraged. If your inner coro (or
`setParentCancelToken`, see `AsyncClosure.md`) throws, that error will (for now -- subject to change)π
mask any `co_cleanup` exceptions.  If some `co_cleanup`s throw, all other
cleanup tasks will still be awaited.  When the inner coro succeeds, but some
cleanups fail, the closure exits with one of the cleanup errors.

### Caveat: Closures may skip `co_cleanup` for just-constructed objects

An object with `co_cleanup` must be safely destructible **without awaiting
cleanup** if the closure that owns it has not yet reached initial suspend.  In
other words, closure setup MAY fail before the `setParentCancelToken` step --
for example, because:
  - Closure argument construction may throw.
  - Closure or `co_cleanup` coro allocation may get `bad_alloc`.
  - Eager setup code in user-supplied `co_cleanup` may fail.

The simplest pattern to provide this guarantee is:
  - Your object does not need cleanup when just-constructed.
  - It is `NonCopyableNonMovable`, and created via
    `as_capture(make_in_place<YourClass>(...))`, so that the user is
    **unable** to accidentally put into in a state requiring cleanup before
    the closure starts.

**NOTE:** There is **no** great alternative to this design.  Yes, it would be
possible to reshuffle most closure setup to happen before your cleanup-capable
object is constructed -- including, even, making `co_cleanup` a static method
that preallocates the cleanup coro with the future address of your object.
However, nothing can work around the fundamental issue, which is that if your
object's constructor throws, it could be in a state requiring **partial** async
cleanup (e.g. of base classes), and this is something that simply can't be
handled properly without "async destructor" support in the language.

### Should `co_cleanup` return once `Task<void>` or a tuple?

`co_cleanup` may return `Task<void>` or a tuple of such tasks. Tuple have
some benefits over sequencing unrelated cleanup tasks via `co_await`:
  - Exception-safety: Allow the cleanups to fail independently.
  - Can save a coro frame allocation.
  - Potentially, enables concurrent cleanup in the future.

**IMPORTANT:** In the future, cleanup tasks from the same tuple may run
concurrently. **Do not use** a tuple unless you are sure that the cleanup tasks
are mutually thread-safe. While the `capture` reference safety system tries to
prevent the most common such errors, the underlying `safe_alias_of_v` heuristic
is easily bypassed.

Example of a cleanup tuple:
```cpp
auto co_cleanup(async_closure_private_t, const exception_wrapper* e) {
  return std::tuple{
    another_cleanup(e),
    // Copy the `e` **pointer** -- it's populated later.
    [=]() -> Task<void> { /* your cleanup here */ }()};
}
```

### Caveat: `co_cleanup` may be a coroutine wrapper

If it is not itself a coroutine, the `co_cleanup()` function will run **eagerly**, when the `async_closure()` is being created. Therefore:
  - All cleanup logic must sit in a `Task<void>` coroutine.
  - If you need to inspect the error, copy the `exception_wrapper*`
    into your task. Do **not** dereference it earlier.
  - For `bad_alloc` safety, it may be a good idea to eagerly preallocate any
    coroutines that your cleanup needs to `co_await`, and to move them into the
    cleanup task returned by your `co_cleanup`. Otherwise, your cleanup may fail
    partially due to OOM.
