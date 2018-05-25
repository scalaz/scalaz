---
layout: docs
title: "FixFree"
---

# FixFree

FixFree is a recursive data structure that either returns a value `A` or returns an effect computation in `F[_]` that yields
another `FixFree`. FixFree can be summarized with following signature:

`type FixFree[F[_], A] = A \/ F[FixFree[F,A]]`

If F is a functor, FixFree[F, A] is a freely-generated monad. FixFree can also be used as fix point defining recursive data 
structures.

## Basics

```tut
import scalaz._, Scalaz._
import data._

val pureFixFree = FixFree.pure[Maybe, Int](1) //Create a FixFree from a pure value using Maybe.
val fromMaybe = FixFree.lift(1.just) //Create a FixFree from an effectful value.
val extracted = FixFree.unwrap(pureFixFree) //Extract a disjunction from a FixFree.
```