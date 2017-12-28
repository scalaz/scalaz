---
layout: docs
title:  "Monoid"
---

# Monoid

**Category Theory**

*A monoid is a semigroup with a unique identity element.*

A monoid instance must satisfy the following laws in addition to those defined by [Semigroup](Semigroup.html):

- Left identity: `append(empty, x) === x`
- Right identity: `append(x, empty) === x`

**Typical imports**
```tut:silent
import scalaz._
import Scalaz._
```

## Instance declaration

```tut
def ListSemigroup[A]: Semigroup[List[A]] = new Semigroup[List[A]] {
  def append(a1: List[A], a2: => List[A]) = a1 ++ a2
}

implicit def ListMonoid[A]: Monoid[List[A]] = new Monoid[List[A]] {
  val semigroup = ListSemigroup[A]
  val empty = List.empty[A]
}
```

Instances can also be defined without the intermediary `Semigroup` by using `MonoidClass`:

```tut
import scalaz.typeclass.MonoidClass

implicit def StringMonoid: Monoid[String] = new MonoidClass[String] {
  def append(a1: String, a2: => String) = a1 + a2
  val empty = ""
}
```

## Usage

```tut
val l1 = List(1, 2, 3)
val l2 = List(4, 5, 6)

val l = l1.append(l2)
l.append(ListMonoid.empty)
```
