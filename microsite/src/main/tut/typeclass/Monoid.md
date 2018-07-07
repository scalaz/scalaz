---
layout: docs
title:  "Monoid"
---

# Monoid [![GitHub](../img/github.png)](https://github.com/scalaz/scalaz/blob/series/8.0.x/base/shared/src/main/scala/scalaz/tc/monoid.scala)

*A monoid is a semigroup with a unique identity element.*

A monoid instance must satisfy the following laws in addition to those defined by [Semigroup](./Semigroup.html):

- Left identity: `append(empty, x) === x`
- Right identity: `append(x, empty) === x`

**Typical imports**
```tut:silent
import scalaz.tc._
import scalaz.Scalaz._
```

# Instance declaration

```tut
{
import scalaz.Scalaz._

implicit def StringMonoid: Monoid[String] = instanceOf(new MonoidClass[String] {
  def mappend(a1: String, a2: => String) = a1 + a2
  val mempty = ""
})
}
```

# Usage

```tut
val s1 = "Hello"
val s2 = " World"

val s = s1.mappend(s2)
s.mappend(Monoid[String].mempty)
```
