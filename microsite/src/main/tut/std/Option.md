---
layout: scalaz
section: std
title:  "Option"
---

Scalaz provides instances for Scala's built-in [Option](https://www.scala-lang.org/api/current/scala/Option.html) such as [Monad](../tc/Monad.html) and [Eq](../tc/Eq.html).

**Typical imports**

```tut:silent
import scala.Option
import scalaz.Scalaz._
```

## Use the type class instances

```tut
val option1: Option[Int] = Some(42)
val option2: Option[Int] = Some(43)

option1 === option2

option2.debug
```
