---
layout: docs
title:  "List"
---

# List [![GitHub](../img/github.png)](https://github.com/scalaz/scalaz/blob/series/8.0.x/std/shared/src/main/scala/collection/list.scala)

Scalaz provides instances for Scala's built-in [List](https://www.scala-lang.org/api/current/scala/collection/immutable/List.html) such as [Monad](../ct/Monad.html) and [Eq](../core/Eq.html).

**Typical imports**

```tut:silent
import scalaz.Scalaz._
import scalaz.std._
```

## Use the type class instances

```tut
val list1: List[Int] = List(1, 2, 3)
val list2: List[Int] = List(1, 2, 3)

val fs: List[Int => Int] = List(_ * 2, _ + 2, _ - 1)

list1 === list2

list1.ap(fs)
```
