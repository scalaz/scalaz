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
import scalaz._, Prelude._, core.EqClass

implicit final def optionEq[A](implicit A: Eq[A]): Eq[Option[A]] =
  instanceOf[EqClass[Option[A]]] {
    case (None, None)         => true
    case (Some(a1), Some(a2)) => A.equal(a1, a2)
    case _                    => false
  }
```
