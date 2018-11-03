---
layout: scalaz
module: base
section: typeclass
source: tc/eq.scala
title:  "Eq"
---

The `Eq` type class describes a type which can be compared for observational equality.

## Instance declaration

```tut
import scalaz.tc._

implicit final def optionEq[A](implicit A: Eq[A]): Eq[Option[A]] =
  instanceOf[EqClass[Option[A]]] {
    case (None, None)         => true
    case (Some(a1), Some(a2)) => A.equal(a1, a2)
    case _                    => false
  }
```

# Laws

What follows is an informal statement of the laws of `Eq`.

A `Eq` must satisfy the following [laws](https://en.wikipedia.org/wiki/Identity_of_indiscernibles):

- Reflexivity:
  - `x === x` for any `x`.
- The indiscernibility of identicals:
  - For any `x` and `y`, if `x === y`, then `x` and `y` are indiscernable
- The identity of indiscernibles:
  - For any `x` and `y`, if `x` and `y` are indiscernable, then `x === y`

*Indiscernability* formalizes the intuitive notion of two objects having the exact same
properties. Two values `x, y: A` are indiscernable if for all functions `f: A => B`
`f(x)` and `f(y)` are equal.

Practically, the way we end up testing indiscernibility of identicals and identity of
indiscernibles is by coming up with predicates that use all of the information inside
`A` values.

These laws entail symmetry and transitivity:

- Symmetry:
  - `x === y` if and only if `y === x`
- Transitivity
  - if `x === y` and `y === z`, then `x === z`

Reflexivity is written as:

```tut
  def reflexivity[A, T](in: A)(assert: Boolean => T)(implicit A: Eq[A]): T =
    assert(A.equal(in, in))
```
