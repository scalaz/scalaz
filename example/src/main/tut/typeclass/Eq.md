---
layout: docs
title:  "Eq"
---

# Eq

*The `Eq` type class describes a type which can be compared for observational equality.*

A `Eq` must satisfy the following [laws](https://en.wikipedia.org/wiki/Identity_of_indiscernibles):

- Reflexivity:
  - `x === x` for any `x`.
- The indiscernibility of identicals:
  - For any `x` and `y`, if `x` is identical to `y`, then `x` and `y` have all the same properties, or in other words there is no function `f: A => Boolean` that returns `true` for `x` and `false` for `y`.
- The identity of indiscernibles:
  - For any `x` and `y`, if `x` and `y` have all the same properties, then `x` is identical to `y`.

These laws entail symmetry and transitivity, which should be easier to test, since they don't universally quantify over all possible predicates in the language:

- Symmetry:
  - `x === y` if and only if `y === x`
- Transitivity
  - if `x === y` and `y === z`, then `x === z`

## Instance declaration

A `Eq` can be declared by `equal`.

```tut
import scalaz._, Prelude._, typeclass.EqClass

implicit final def optionEq[A](implicit A: Eq[A]): Eq[Option[A]] =
  instanceOf[EqClass[Option[A]]](
    (a1, a2) =>
      (a1, a2) match {
        case (None, None)         => true
        case (Some(a1), Some(a2)) => A.equal(a1, a2)
        case _                    => false
    }
  )
```
