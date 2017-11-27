---
layout: docs
title:  "Disjunction"
---

# Disjunction

`Disjunction` is a data type used to signal multiple possible outcomes of a computation.
It can be most useful when dealing with exceptional and errors but is not tied to these cases.

**Typical imports**

```tut:silent
import scalaz.Prelude._
import scalaz.data.Disjunction._
import scalaz.data.Disjunction.Syntax._
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

