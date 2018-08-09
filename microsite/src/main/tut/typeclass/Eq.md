---
layout: docs
section: typeclass
title:  "Eq"
---

# Eq [![GitHub](../img/github.png)](https://github.com/scalaz/scalaz/blob/series/8.0.x/base/shared/src/main/scala/scalaz/core/eq.scala)

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
`f(x)` and `f(y)` are indiscernable.

These laws entail symmetry and transitivity:

- Symmetry:
  - `x === y` if and only if `y === x`
- Transitivity
  - if `x === y` and `y === z`, then `x === z`

In code, indiscernability of identicals and identity of
indiscernibles are written in a single law:

```tut
  def identity[A, B, T](fst: A, snd: A)(f: A => B)(assert: (Boolean, B, B) => T)(implicit A: Eq[A]): T =
    assert(A.equal(fst, snd), f(fst), f(snd))
```

Essentially stating that `f(fst)` and `f(snd)` must agree
with `fst === snd`.

Reflexivity is written as:

```tut
  def reflexivity[A, T](in: A)(assert: Boolean => T)(implicit A: Eq[A]): T =
    assert(A.equal(in, in))
```
