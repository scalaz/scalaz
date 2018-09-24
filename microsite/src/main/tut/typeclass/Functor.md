---
layout: docs
section: typeclass
title:  "Functor"
---

# Functor [![GitHub](../img/github.png)](https://github.com/scalaz/scalaz/blob/series/8.0.x/base/shared/src/main/scala/scalaz/tc/functor.scala)

A way to map ordinary functions under type constructors,
turning `A => B` to `F[A] => F[B]` with `Functor[F]`.

**Typical imports**

```tut:silent
import scalaz.tc._
import scalaz.Predef._
import scalaz.Scalaz._
```

# Instance declaration

```tut
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

# Law

The sole law required for a valid `Functor` is the "identity" law:

```tut
def identityToIdentity[F[_], A, T](in: F[A])(assert: (F[A], F[A]) => T)(implicit F: Functor[F]): T =
  assert(in, F.map(in)(identity))
```

The implication of this law, because `map` can't tell which function it's being called
with, is that `map` doesn't modify the `F[_]` context.

A free law implied by this law is that `map` respects composition of functions:

```tut
def freeLawComposition[F[_], A, B, C, T](in: F[A])(f: A => B, g: B => C)(assert: (F[C], F[C]) => T)(implicit F: Functor[F]): T =
  assert(
    F.map(in)(f.andThen(g)),
    F.map(F.map(in)(f))(g)
  )
```

That is to say, two `map` calls can be replaced by a single `map` call
passed the composite of the mapped functions.
