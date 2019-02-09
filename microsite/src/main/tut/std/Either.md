---
layout: scalaz
section: std
title:  "Either"
---

Scalaz provides instances for Scala's built-in [Either](https://www.scala-lang.org/api/current/scala/util/Either.html) such as [Monad](../tc/Monad.html) and [Eq](../tc/Eq.html).

**Typical imports**

```tut:silent
import scalaz.{Predef, Scalaz}
import Predef._, Scalaz._
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

either1.toString
either2.toString
```
