---
layout: docs
title:  "Option"
---

# Option [![GitHub](../img/github.png)](https://github.com/scalaz/scalaz/blob/series/8.0.x/std/shared/src/main/scala/option.scala)

Scalaz provides instances for Scala's built-in [Option](https://www.scala-lang.org/api/current/scala/Option.html) such as [Monad](../ct/Monad.html) and [Eq](../core/Eq.html).

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
