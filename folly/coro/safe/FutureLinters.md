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

  - (hi-pri, moderate) Auto-replace instances of `Task` by `NowTask` or
    `ValueTask` whenever possible.

  - (hi-pri, complex) Detects when you pass a type into `SafeTask`,
    `async_closure`, or similar, but `safe_alias_of_v` for that type is not
    (recursively) correct. Offer efficient auto-correct.  This would drive
    annotations for:
      * User types that contain unsafe, or limited-safety aliasing
      * Common generic containers
    The big caveat is that supporting each generic container will require
    fixing up existing code that (wrongly) assumes the contained data is
    safe.  On the bright side, this may catch legitimate safety bugs.
    This would also require a "usability refactor" of `safe_alias_of_`,
    in a separate section below.

  - (mid-pri, easy) Special-case of the prior bullet -- since `capture`
    relies on `safe_alias`, do not place `capture` refs into containers
    that do not properly specialize `safe_alias_for_`.

  - (mid-pri, easy) Fail when `MemberTask` is used for coros that are
    **not** non-static member functions.

  - (mid-pri, medium) Plug the "callable hole": When a function invokes a
    stateful callable that could contain references to outside the function
    (passed as an arg, or otherwise contains external references),
    require the args to that callable to have `maybe_value` safety.

  - (mid-pri, easy) Detect when a `capture<Val>` wrapper is being moved outside
    of the current coro, and raise a blocking lint error. Moving the wrapper
    would breaks a core safety invariant, see `LifetimeSafetyDesign.md`.

  - (mid-pri, easy) It should not be common that values contained in `capture`s
    are moved. However, when it is necessary, use `*std::move(arg)` (good)
    instead of `std::move(*arg)` (bad).  The former benefits from use-after-move
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

### Improve `safe_alias_for_` usability for the safety-annotation linter

Today, changing the value of `folly::safe_alias_of_v` requires specializing the
struct `folly::detail::safe_alias_for_`. This is highly verbose.

At a minimum, we could easily switch the implementation to be a variable
template, and move it out of `folly::detail`. Then, a typical use-case would
look like this:

```cpp
} // namespace otherNs

// Actually, this would be `#include <folly/coro/safe/SafeAlias-fwd.h>`
template <typename>
inline constexpr auto folly::safe_alias_for_;

template <>
inline constexpr auto folly::safe_alias_for_<otherNs::YourType> =
    folly::safety_level::safe;

namespace otherNs {
```

This is still verbose, but it has some nice properties:
  - Forward-declaring the primary template helps catch typos.
  - The identifier is namespaced, preventing collisions & avoiding "oops I put
    it in the wrong namespace" errors.

An alternative would be a "function + ADL", i.e.

```cpp
class YourType {};
auto folly_adl_safe_alias_for(YourType)
    -> folly::safe_alias_constant<folly::safe_alias::unsafe>;
// OR
consteval auto folly_adl_safe_alias_for(YourType) {
  return folly::safe_alias::unsafe;
}
```

This is quite concise, but has the usual downsides of ADL:
  - Naming collisions -- these don't seem very likely here.
  - Easy to declare in the wrong namespace (should be the same namespace as the
    target type) -- mitigated by the linter that will check for these in the
    first place.
  - Surprising interactions with template metaprogramming -- hopefully would be
    caught by tests?
  - It is probable that this has a slightly higher compile-time cost than a
    variable template. It might be worth comparing "function declaration" vs
    "function definition" approaches for speed.

The best approach of the two isn't clear to me, and could be decided by the
linter author.

Either of these two approaches is additionally compatible with checking for a
static class member, like so:

```
constexpr static safe_alias folly_safe_alias = safe_alias::unsafe;
```

This last approach is maximally brief, and would be worth supporting for user
classes. However, we still need a generic-programming approach as well, in order
to handle things like containers.

So, we'd likely support both, and either check they don't conflict, or pick the
less safe level of the two.
