---
layout: docs
section: std
title:  "List"
---

# List [![GitHub](../img/github.png)]

Scalaz provides instances for Scala's built-in [List](https://www.scala-lang.org/api/current/scala/collection/immutable/List.html) such as [Monad](../tc/Monad.html) and [Eq](../tc/Eq.html).

**Typical imports**

```tut:silent
import scalaz.Scalaz._
```

## Use the type class instances

```tut
val list1: List[Int] = List(1, 2, 3)
val list2: List[Int] = List(1, 2, 3)

val fs: List[Int => Int] = List(_ * 2, _ + 2, _ - 1)

list1 === list2

list1.ap(fs)
```
