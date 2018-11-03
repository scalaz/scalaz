---
layout: scalaz
section: std
title:  "Vector"
---

Scalaz provides instances for Scala's built-in [Vector](https://www.scala-lang.org/api/current/scala/collection/immutable/Vector.html) such as [Monad](../tc/Monad.html) and [Eq](../tc/Eq.html).

**Typical imports**

```tut:silent
import scalaz.Scalaz._
```

## Use the type class instances

```tut
val vector1: Vector[Int] = Vector(1, 2, 3)
val vector2: Vector[Int] = Vector(1, 2, 3)

val fs: Vector[Int => Int] = Vector(_ * 2, _ + 2, _ - 1)

vector1 === vector2

vector1.ap(fs)
```
