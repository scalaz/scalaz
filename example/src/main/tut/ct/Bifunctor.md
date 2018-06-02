---
layout: docs
title:  "Bifunctor"
---

# Bifunctor

*The `Bifunctor` type class describes a type which has two type parameters,
 each of which acts as a (covariant) [`Functor`](./Functor.html).*

A `Bifunctor` must satisfy the following laws:

- the `Functor` laws on the left:
  - `lmap(elem)(identity) === elem`
  - `lmap(elem)(f compose g) === lmap(lmap(elem(g)))(f)`
- the `Functor` laws on the right:
  - `rmap(elem)(identity) === elem`
  - `rmap(elem)(f compose g) === rmap(rmap(elem(g)))(f)`
- and a coherency law:
  - `rmap(lmap(elem)(f))(g) === lmap(rmap(elem)(g))(f) == bimap(elem)(f, g)`

## Instance declaration

A `Bifunctor` can be declared by specifying either `bimap`, or both `lmap` and `rmap`.

```tut
import scalaz._, Prelude._, ct.BifunctorClass, data.These

val tuple2Bifunctor: Bifunctor[Tuple2] =
  instanceOf(new BifunctorClass[Tuple2] with BifunctorClass.DeriveBimap[Tuple2] {
    def lmap[A, B, S](fab: (A, B))(f: A => S): (S, B) = fab.copy(_1 = f(fab._1))
    def rmap[A, B, T](fab: (A, B))(f: B => T): (A, T) = fab.copy(_2 = f(fab._2))
  })

val theseBifunctor: Bifunctor[These] =
  instanceOf(new BifunctorClass[These] with BifunctorClass.DeriveLmapRmap[These] {
    def bimap[A, B, S, T](fab: A \&/ B)(as: A => S, bt: B => T): S \&/ T = fab.bimap(as)(bt)
  })
```
