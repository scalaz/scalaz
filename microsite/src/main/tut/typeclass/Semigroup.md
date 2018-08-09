---
layout: docs
title:  "Semigroup"
---

# Semigroup [![GitHub](../img/github.png)](https://github.com/scalaz/scalaz/blob/series/8.0.x/base/shared/src/main/scala/scalaz/tc/semigroup.scala)

*A semigroup is an algebraic structure consisting of a set and an associative binary operation.*

A semigroup instance needs to satisfy the following law:

- Associativity: `append(x, append(y, z)) === append(append(x, y), z)`

**Typical imports**

```tut:silent
import scalaz.tc._
import scalaz.Scalaz._
```

# Instance declaration

```tut
implicit val intSemigroup: Semigroup[Int] = instanceOf(new SemigroupClass[Int] {
  def mappend(a1: Int, a2: => Int) = a1 + a2
})
```

# Usage

```tut
Semigroup[Int].mappend(1, 2)
1.mappend(2)
```

# Law

The sole law for a valid `Semigroup` is that `mappend` is associative:

```tut


```
