---
layout: docs
title:  "Eq"
---

# Eq [![GitHub](../img/github.png)](https://github.com/scalaz/scalaz/blob/series/8.0.x/base/shared/src/main/scala/scalaz/core/eq.scala)

*The `Eq` type class describes a type which can be compared for observational equality.*

A `Eq` must satisfy the following [laws](https://en.wikipedia.org/wiki/Identity_of_indiscernibles):

- Reflexivity:
  - `x === x` for any `x`.
- The indiscernibility of identicals:
  - For any `x` and `y`, if `x === y`, then `x` and `y` are indiscernable
- The identity of indiscernibles:
  - For any `x` and `y`, if `x` and `y` are indiscernable, then `x === y`

*Indiscernability* formalizes the intuitive notion of two objects having the exact same properties. Two values `x, y: A` are indiscernable if there exists no function `f: A => Boolean` such that `f(x)` is `true` and `f(y)` is `false`.

These laws entail symmetry and transitivity, which should be easier to test, since they don't universally quantify over all possible predicates in the language:

- Symmetry:
  - `x === y` if and only if `y === x`
- Transitivity
  - if `x === y` and `y === z`, then `x === z`

## Instance declaration

```tut
import scalaz._, Prelude._, core.{ EqAnyRef, EqClass }

implicit final def optionEq[A](implicit A: Eq[A]): Eq[Option[A]] =
  instanceOf(new EqAnyRef[Option[A]] {
    def valueEqual(first: Option[A], second: Option[A]) = (first, second) match {
    case (None, None)         => true
    case (Some(a1), Some(a2)) => A.equal(a1, a2)
    case _                    => false
  }}: EqClass[Option[A]])
```

### EqClass vs EqAnyRef

Scalaz contains two possible traits to extend when declaring an `Eq` instance: `EqClass` and `EqAnyRef`.
`EqAnyRef` is a specialization of `EqClass` that will always run a reference check before handing control to the user-supplied equality logic.
`EqAnyRef` should be preferred in most cases except for `AnyVal` subtypes. In those cases, the `@specialized` annotation can be used to avoid boxing.
