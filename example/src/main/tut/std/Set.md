---
layout: docs
title:  "Set"
---

# Set

Scalaz provides instances for Scala's built-in `Set` such as [Eq](../typeclass/Eq.html).

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
