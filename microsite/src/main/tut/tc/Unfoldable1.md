---
layout: scalaz
module: base
section: typeclass
source: tc/unfoldable1.scala
title:  "Unfoldable1"
---

* `Unfoldable1` identifies non-empty data structures which can be unfolded*, and it's a more general form than `Unfoldable`.
Given a seed value `b` of type `B`, `unfoldRight1(f)(b)` let's you define how to generate (*unfold*) a non-empty data structure of type `F[A]`.
The generating function `f` always return a value, and then optionally a value to continue unfolding from.

**Typical imports**

```tut:silent
import scalaz.tc._
import scalaz.data._
import scalaz.Scalaz._
```

# Usage

TBD A sample with NonEmptyList (when they're available in scalaz 8), like:

```scala
Unfoldable[NonEmptyList].unfoldRight1[Int, Int](b => if (b == 0) (b, empty) else (b, just(b - 1)))(10)
```
