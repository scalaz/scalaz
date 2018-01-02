---
layout: docs
title:  "Disjunction"
---

# Disjunction

`Disjunction` is a data type used to signal multiple possible outcomes of a computation.
It is often used when dealing with errors, but is not tied to that case.

**Typical imports**

```tut:silent
import scalaz.Scalaz._
```

## Construction

```tut
val l: String \/ Int = "foo".left
val r: Long \/ Int = 42.right
```

## Pattern matching

```tut
l match {
  case -\/(left) => println(left)
  case \/-(right) => println(right)
}
```

## Use the Monad instance

```tut
r.flatMap { right =>
  \/-(right)
}
```

