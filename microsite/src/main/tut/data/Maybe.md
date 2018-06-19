---
layout: docs
section: data
title:  "Maybe"
---

# Maybe [![GitHub](../img/github.png)](https://github.com/scalaz/scalaz/blob/series/8.0.x/base/shared/src/main/scala/scalaz/data/maybe.scala)

`Maybe` can either signal the presence or absence of a value and is a safer alternative to [scala.Option](https://www.scala-lang.org/api/current/scala/Option.html).
`Maybe` does not provide unsafe methods such as `Option.get` and `Option.head`, both of which may throw exceptions.

**Typical imports**

```tut:silent
import scalaz.Scalaz._
```

# Creation

```tut
"Hello World".just
empty[Int]
```

# Use the type class instances

```tut
val x = just(50L)
val y = "Hello".pure[Maybe]

y.map(_ + " World")

x.flatMap { y =>
  just(2 * y)
}
```

# Pattern matching

```tut
x match {
  case Empty() => println("No value")
  case Just(x) => println(x)
}
```
