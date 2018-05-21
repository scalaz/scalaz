---
layout: docs
title:  "Ord"
---

# Ord

*Ord is used for totally ordered datatypes.*

Ord builds on top of [Eq](./Eq.html) in that it adds the concept of ordering values.
A value can be "greater than", "less than" or "equal to" another value.


**Typical imports**

```tut:silent
import scalaz.Scalaz._
```

## Instance declaration

```tut
import scalaz.algebra.OrdClass

implicit val intOrd: Ord[Int] = instanceOf(new OrdClass[Int] {
  def comp(a: Int, b: Int): Ordering = (a, b) match {
    case (x, y) if (x < y) => LT
    case (x, y) if (x > y) => GT
    case _ => EQ
  }
})
```

## Usage

```tut
val a = 5
val b = 7
val c = 5

a comp b
a comp c
b comp c
```
