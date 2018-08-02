---
layout: docs
section: std
title:  "Tuple"
---

# Tuple [![GitHub](../img/github.png)]

Scalaz provides instances for Scala's built-in `Tuple` variants ([Tuple2](https://www.scala-lang.org/api/current/scala/Tuple2.html) through [Tuple22](https://www.scala-lang.org/api/current/scala/Tuple22.html)) such as [Monad](../tc/Monad.html) and [Eq](../tc/Eq.html).

**Typical imports**

```tut:silent
import scalaz.Scalaz._
```

## Use the type class instances

```tut
val tuple1: Tuple2[Int, String] = (42, "scalaz")
val tuple2: Tuple2[Int, String] = (1, "scalaz")

tuple2 === tuple1
```
