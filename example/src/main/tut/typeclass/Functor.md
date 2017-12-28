---
layout: docs
title:  "Functor"
---

# Functor

The `Functor` type class represents a type that can be mapped over.

**Category Theory**

*A (mathematical) functor represents a mapping between categories.*

A functor needs to satisfy the two functor laws:

- `Functor.map(elem)(identity) === elem`
- `Functor.map(elem)(f.andThen(g)) === Functor.map(Functor.map(elem)(f))(g)`

**Typical imports**

```tut:silent
import scalaz._
import Scalaz._
```

## Instance declaration

```tut
val listFunctor: Functor[List] = new Functor[List] {
  def map[A, B](ma: List[A])(f: A => B): List[B] = ma match {
    case Nil => Nil
    case x :: xs => f(x) :: map(xs)(f)
  }
}
```

## Usage

[Maybe](../data/Maybe.html) does not have a `map` method. Instead its `Functor` is used, which is implicitly available from the above imports.

```tut
val x: Maybe[Long] = just(50L)
x.map(_ - 8L)
```
