---
layout: docs
title:  "Functor"
---

# Functor

*The `Functor` type class represents a type that can be mapped over.*

A functor needs to satisfy the two functor laws:

- `functor.map(elem)(identity) === elem`
- `functor.map(elem)(f.andThen(g)) === functor.map(functor.map(elem)(f))(g)`

**Typical imports**

```tut:silent
import scalaz.Scalaz._
```

## Instance declaration

In order to implement a `Functor` instance, the following interface needs to be met:

```scala
trait Functor[F[_]] {
  def map[A, B](ma: F[A])(f: A => B): F[B]
}
```

**Scalaz** offers a variety of type class instances by default but as an example, let us (naïvely) re-implement the `Functor` for `List`:

```tut
val listFunctor: Functor[List] = new Functor[List] {
  def map[A, B](ma: List[A])(f: A => B): List[B] = ma match {
    case Nil => Nil
    case x :: xs => f(x) :: map(xs)(f)
  }
}
```

We can now use the `map` method on the `Functor` for our instances of `List`.

## Usage

[Maybe](../data/Maybe.html) does not have a `map` method. Instead its `Functor` is used, which is implicitly available from the above imports.

```tut
val x: Maybe[Long] = just(50L)
x.map(_ - 8L)
```
