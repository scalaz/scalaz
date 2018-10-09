---
layout: scalaz
module: base
section: data
source: data/maybe.scala
title:  "Maybe"
---

`Maybe` can either signal the presence or absence of a value and is a safer alternative to [scala.Option](https://www.scala-lang.org/api/current/scala/Option.html).
`Maybe` does not provide unsafe methods such as `Option.get` and `Option.head`, both of which may throw exceptions.

**Typical imports**

```tut:silent
import scalaz.Predef._
import scalaz.data._
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
import scala.Predef.println
x match {
  case Maybe.Empty() => println("No value")
  case Maybe.Just(x) => println(x)
}
```
