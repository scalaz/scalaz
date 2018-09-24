---
layout: docs
section: typeclass
title:  "Monoid"
---

# Monoid [![GitHub](../img/github.png)](https://github.com/scalaz/scalaz/blob/series/8.0.x/base/shared/src/main/scala/scalaz/tc/monoid.scala)

A monoid is a semigroup with identity.

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

# Law

A monoid instance must satisfy laws that make `mempty` into an identity for
`mappend`, in addition to the [Semigroup](./Semigroup.html) law:

```tut
def leftIdentity[A, T](in: A)(assert: (A, A) => T)(implicit A: Monoid[A]): T =
  assert(in, A.mappend(A.mempty, in))

def rightIdentity[A, T](in: A)(assert: (A, A) => T)(implicit A: Monoid[A]): T =
  assert(in, A.mappend(in, A.mempty))
```
