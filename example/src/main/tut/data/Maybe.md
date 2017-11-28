---
layout: docs
title:  "Maybe"
---

# Maybe

`Maybe` can either signal the presence or absence of a value and is a safer alternative to `scala.Option`.
`Maybe` does not provide unsafe method such as `Option.get` and `Option.head`, both of which may throw exceptions.

**Typical imports**

```tut:silent
import scalaz.Prelude._
```

## Creation

```tut
just("Hello World")
empty[Int]
```

## Use the Monad instance

```tut
val x: Maybe[Long] = just(50L)
x.flatMap { y =>
  just(2 * y)
}
```
