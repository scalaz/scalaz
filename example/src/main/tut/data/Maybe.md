---
layout: docs
title:  "Maybe"
---

# Maybe

`Maybe` can either signal the presence or absence of a value and is a safer alternative to `scala.Option`.
`Maybe` does not provide unsafe methods such as `Option.get` and `Option.head`, both of which may throw exceptions.

**Typical imports**

```tut:silent
import scalaz.Scalaz._
```

## Creation

```tut
"Hello World".just
empty[Int]
```

## Use the type class instances

```tut
val x = just(50L)
val y = "Hello".pure[Maybe]

y.map(_ + " World")

x.flatMap { y =>
  just(2 * y)
}
```

## Pattern matching

```tut
x match {
  case Empty() => println("No value")
  case Just(x) => println(x)
}
```
