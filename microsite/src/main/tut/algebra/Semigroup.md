---
layout: docs
section: typeclass
title:  "Semigroup"
---

# Semigroup [![GitHub](../img/github.png)](https://github.com/scalaz/scalaz/blob/series/8.0.x/base/shared/src/main/scala/scalaz/algebra/semigroup.scala)

*A semigroup is an algebraic structure consisting of a set and an associative binary operation.*

A semigroup instance needs to satisfy the following law:

- Associativity: `mappend(x, mappend(y, z)) === mappend(mappend(x, y), z)`

**Typical imports**

```tut:silent
import scalaz.Scalaz._
```

# Instance declaration

```tut
import scalaz.Prelude._
import scalaz.algebra.SemigroupClass

implicit val StringSemigroup: Semigroup[String] = instanceOf(new SemigroupClass[String] {
  def mappend(a1: String, a2: => String) = a1 + a2
})
```

# Usage

```tut
Semigroup[String].mappend("hello", "world")
"hello".mappend("world")
```
