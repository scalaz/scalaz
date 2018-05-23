---
layout: docs
title:  "Option"
---

# Option

Scalaz provides instances for Scala's built-in `Option` such as [Monad](../ct/Monad.html) and [Eq](../core/Eq.html).

**Typical imports**

```tut:silent
import scalaz.Scalaz._
import scalaz.std._
```

## Use the type class instances

```tut
val option1: Option[Int] = Some(42)
val option2: Option[Int] = Some(43)

option1 === option2

option2.debug
```
