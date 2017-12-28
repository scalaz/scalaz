---
layout: docs
title:  "Semigroup"
---

# Semigroup

*A semigroup is an algebraic structure consisting of a set and an associative binary operation.*

A semigroup instance needs to satisfy the following law:

- Associativity: `append(x, append(y, z)) === append(append(x, y), z)`

**Typical imports**
```tut:silent
import scalaz._
import Scalaz._
```

## Instance declaration

```tut
implicit val StringSemigroup = new Semigroup[String] {
  def append(a1: String, a2: => String) = a1 + a2
}
```

## Usage

```tut
StringSemigroup.append("hello", "world")
"hello".append("world")
```
