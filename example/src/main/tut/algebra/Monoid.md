---
layout: docs
title:  "Monoid"
---

# Monoid

*A monoid is a semigroup with a unique identity element.*

A monoid instance must satisfy the following laws in addition to those defined by [Semigroup](./Semigroup.html):

- Left identity: `append(empty, x) === x`
- Right identity: `append(x, empty) === x`

**Typical imports**
```tut:silent
import scalaz.Scalaz._
```

## Instance declaration

```tut
{
import scalaz.Prelude._
import scalaz.algebra.MonoidClass

implicit def StringMonoid: Monoid[String] = instanceOf(new MonoidClass[String] {
  def append(a1: String, a2: => String) = a1 + a2
  val empty = ""
})
}
```

## Usage

```tut
val s1 = "Hello"
val s2 = " World"

val s = s1.append(s2)
s.append(Monoid[String].empty)
```
