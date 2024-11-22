## Linting is part of the long-term plan

`folly/coro/safe` is meant to be safe & usable with the compiler alone.
However, linters can ease the learning curve for using the framework effectively.

## Existing linters

A couple of "standard modern C++ linters" are assumed to be active:
  - Use-after-move linting improves safety in a number of places, from
    `AsyncObject` slots, to `capture<V&&>` usage.
  - Initializer order linting helps ensure that `AsyncObject` slots are wired up
    correctly.

## Desired future linters

### Safety type-system: `SafeTask`, `safe_alias_of_v`, `capture`, etc

  - (hi-pri, complex) Detects when you pass a type into `SafeTask`,
    `async_closure`, or similar, but `safe_alias_v` for that type is not
    (recursively) correct.  This would drive annotations for:
      * User types that contain unsafe, or limited-safety aliasing
      * Common generic containers
    The big caveat is that supporting each generic container will require
    fixing up existing code that (wrongly) assumes the contained data is
    safe.  On the bright side, this may catch legitimate safety bugs.

  - (mid-pri, easy) Special-case of the prior bullet -- since `capture`
    relies on `safe_alias`, do not place `capture` refs into containers
    that do not properly specialize `safe_alias_for_`.

  - (mid-pri, easy) Fail when `MemberTask` is used for coros that are
    **not** non-static member functions.

  - (mid-pri, medium) Plug the "callable hole": When a function invokes a
    stateful callable that could contain references to outside the function
    (passed as an arg, or otherwise contains external references),
    require the args to that callable to have `maybe_value` safety.

  - (mid-pri, easy) It should not be common that `capture`s are moved.
    However, when it is necessary, use `*std::move(arg)` (good) instead of
    `std::move(*arg)` (bad).  The former benefits from use-after-move
    linting, and (potentially) future runtime checks.

### `AsyncObject`

  - (mid-pri, easy) Constructors of classes derived from `AsyncObject`
    should always take `AsyncObjectTag` as the first argument, and should
    only take arguments by-value (i.e.  if you want a reference, use a
    `capture` reference).

  - (mid-pri, easy) When accessing a slot via `ptr->slotName_(ptr)`, enforce
    that the same `ptr` identifier is used in both places.

  - (mid-pri, easy) Ensure that `AsyncObject::Slot::nextTag()` is only
    used in initializers, and only used once.
    **FIXME:** Maybe drop this if I switch to a member var `nextTag` and
    use-after-move actually works?
