---
layout: docs
title:  "Option"
---

# Option

Scalaz provides instances for Scala's built-in `Tuple` variants (`Tuple2` through `Tuple22`) such as [Monad](../typeclass/Monad.html) and [Eq](../typeclass/Eq.html).

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
