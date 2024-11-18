Lifecycle of AsyncObjectPtr-based objects:

`AsyncObject` destruction can be delayed -- its dtor COULD run as late as
the cleanup of the parent closure, whether the object is owned by the
closure, or whether it's an RAII pointer inside the closure.  Destructors
can access members, so, the safe_alias rules for object ctor args are
basically the same as those of coros scheduled on a safe scope:
  - It can ONLY take inputs with safety >= co_cleanup_safe_ref.
  - If you are tempted to special-case the XXX `thisAsyncScope` marker to
    reference the parent scope, read "POTENTIALLY background work" below.

RAII AsyncObjectPtr may get destructed before the current closure is done,
meaning that coros that access its `this` pointer are aliasing-**unsafe** --
good only for immediately-awaited `Task`, **not** for scheduling on
background scopes.
  => XXX UX solution: make "unsafe" tasks nonmovable, requiring them to be
     awaited right away. ... or why not "plain tasks" for now?

There are many approaches to backgrounding `AsyncObject` coros, most bad.
  - [BEST PRACTICE] It's fine, and encouraged, to background coros on the
    object's own scopes.  You can skip reading the other bullet points.
      * XXX how to support this?  very similar problem to the below
        "mechanic for creating a SafeTask out of a coro method on an object
        for `capture`"
  - If the object is managed by an RAII `Ptr`, there's no good, static way
    to ensure the coro finishes before the dtor runs -- and SafeTask should
    ensure it.  Possible future workarounds:
      * (single-use) Move the unique ptr into the coro.
      * (complex, adds overhead) Support shared_ptr semantics.
  - If the object is owned by a closure, there's another option:
      * We can easily background work on sub-scopes that are guaranteed to
        be joined before the closure starts cleanup.
          - It's a bit annoying to use, but almost works already.
          - We'll need to support making objects as an `capture` XXX, or
            moving `Ptr`s into them.  Then, the parent closure owns the
            object & sees a `body_only_ref` ref.  We pass an `capture<ref>`
            to a child closure that is NOT `shared_cleanup` -- at which
            point the ref gets upgraded to `co_cleanup_safe_ref`, and can be passed
            into a scope of the child.
      * We can POTENTIALLY background work on the dtor scope (or, less
        usefully, with more complexity, on its closure cleanup siblings).
          - XXX what cleanup ordering? it's unspecified.
          - This depends on solving some cleanup ordering issues -- we DO
            NOT want the background work to run after the object's dtor
            runs, and this is very possible if they are on the same scope.
          - One idea is to implicitly create a sub-scope for
            XXX `thisAsyncScope` -- dubious UX, why not `safeAsyncScope()`?
          - Short of shared_ptr semantics, I _think_ other way of ordering
            the dtor awaitable to start after the object coros finish would
            be equivalent to a sub-scope.  For example, we can replace the
            object's `Baton` with `Barrier`, and use `barrier->add(1)` at
            the start of each background task / `barrier->arrive()` at the
            end, this seems to just reinvent `AsyncScope`.



XXX Work out safe invocation mechanics for:
  - Backgrounding method coros on scopes owned by the current object (should
    work for both `Ptr` and `capture` storage).
  - In essence, should make an `capture<Ref>` from `this` the way that
    we do already.
