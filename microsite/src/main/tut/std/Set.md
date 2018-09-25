---
layout: scalaz
section: std
title:  "Set"
---

Scalaz provides instances for Scala's built-in [Set](https://www.scala-lang.org/api/current/scala/collection/immutable/Set.html) such as [Eq](../tc/Eq.html).

**Typical imports**

```tut:silent
import scala.Predef.Set
import scalaz.{Predef, Scalaz}
import Predef._, Scalaz._
```

## Use the type class instances

```tut
val set1: Set[Int] = Set(1, 2, 3)
val set2: Set[Int] = Set(1, 2, 3, 3)

set1 === set2
```
