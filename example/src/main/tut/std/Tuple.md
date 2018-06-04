---
layout: docs
title:  "Tuple"
---

# Tuple [![GitHub](../img/github.png)](https://github.com/scalaz/scalaz/blob/series/8.0.x/std/shared/src/main/scala/tuple.scala)

Scalaz provides instances for Scala's built-in `Tuple` variants ([Tuple2](https://www.scala-lang.org/api/current/scala/Tuple2.html) through [Tuple22](https://www.scala-lang.org/api/current/scala/Tuple22.html)) such as [Monad](../ct/Monad.html) and [Eq](../core/Eq.html).

**Typical imports**

```tut:silent
import scalaz.Scalaz._
import scalaz.std._
```

## Use the type class instances

```tut
val tuple1: Tuple2[Int, String] = (42, "scalaz")
val tuple2: Tuple2[Int, String] = (1, "scalaz")

tuple2 === tuple1
```
