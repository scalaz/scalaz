---
layout: docs
title:  "Functor"
---

# Functor [![GitHub](../img/github.png)](https://github.com/scalaz/scalaz/blob/series/8.0.x/base/shared/src/main/scala/scalaz/ct/functor.scala)

*The `Functor` type class represents a type that can be mapped over.*

A functor needs to satisfy the two functor laws:

- `Functor.map(elem)(identity) === elem`
- `Functor.map(elem)(f.andThen(g)) === Functor.map(Functor.map(elem)(f))(g)`

**Typical imports**

```tut:silent
import scalaz.Scalaz._
```

# Instance declaration

```tut
import scalaz.Prelude._
import scalaz.ct.FunctorClass

val listFunctor: Functor[List] = instanceOf(new FunctorClass[List] {
  def map[A, B](ma: List[A])(f: A => B): List[B] = ma match {
    case Nil => Nil
    case x :: xs => f(x) :: map(xs)(f)
  }
})
```

# Usage

[Maybe](../data/Maybe.html) does not have a `map` method. Instead its `Functor` is used, which is implicitly available from the above imports.

```tut
val x = just(50L)
x.map(_ - 8L)
```
