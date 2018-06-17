---
layout: docs
title:  "Set"
---

# Set [![GitHub](../img/github.png)](https://github.com/scalaz/scalaz/blob/series/8.0.x/std/shared/src/main/scala/collection/set.scala)

Scalaz provides instances for Scala's built-in [Set](https://www.scala-lang.org/api/current/scala/collection/immutable/Set.html) such as [Eq](../core/Eq.html).

**Typical imports**

```tut:silent
import scalaz.Scalaz._
import scalaz.std._
```

## Use the type class instances

```tut
val set1: Set[Int] = Set(1, 2, 3)
val set2: Set[Int] = Set(1, 2, 3, 3)

set1 === set2
```
