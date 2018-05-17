---
layout: docs
title:  "List"
---

# List

Scalaz provides instances for Scala's built-in `List` such as [Monad](../typeclass/Monad.html) and [Eq](../typeclass/Eq.html).

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
