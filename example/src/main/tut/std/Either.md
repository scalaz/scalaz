---
layout: docs
title:  "Either"
---

# Either

Scalaz provides instances for Scala's built-in `Either` such as [Monad](../typeclass/Monad.html) and [Eq](../typeclass/Eq.html).

**Typical imports**

```tut:silent
import scalaz.Scalaz._
import scalaz.std._
```

## Use the type class instances

```tut
val either1: Either[String, Int] = Right(1)
val either2: Either[String, Int] = Left("scalaz")

val fs: Either[String, Int => Int] = Right(_ * 2)

either1 === either2

either1.ap(fs)

either2.bimap(
 left => left.toUpperCase,
 right => right * 10
)

either1.debug
either2.debug
```
