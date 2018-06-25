---
layout: docs
section: std
title:  "List"
---

# List [![GitHub](../img/github.png)](https://github.com/scalaz/scalaz/blob/series/7.3.x/core/src/main/scala/scalaz/std/List.scala)

Scalaz provides instances for Scala's built-in [List](https://www.scala-lang.org/api/current/scala/collection/immutable/List.html) such as [Monad](../typeclass/Monad.html) and [Equal](./Equal.html).

**Typical imports**

```tut:silent
import scalaz._
import Scalaz._
```

## Use the type class instances

```tut
val list1: List[Int] = List(1, 2, 3)
val list2: List[Int] = List(1, 2, 3)

list1 === list2

list2.map(_ + 2)
```
