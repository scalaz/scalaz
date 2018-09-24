---
layout: docs
section: typeclass
title:  "Monad"
---

# Monad [![GitHub](../img/github.png)](https://github.com/scalaz/scalaz/blob/series/8.0.x/base/shared/src/main/scala/scalaz/tc/monad.scala)

A monad is a subclass of `Bind`, which adds an identity, `pure`,
by also being a subclass of `Applicative`.

A monad is a semi-monad with identity.

**Typical imports**

```tut:silent
import scalaz.Predef._
import scalaz.tc._
import scalaz.Scalaz._
```

# Instance declaration

```tut
val listMonad: Monad[List] =
  instanceOf(new MonadClass[List] {
    override def pure[A](a: A): List[A] = List(a)
    override def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)
    override def flatten[A](ma: List[List[A]]): List[A] = ma.flatMap(x => x)
    override def map[A, B](ma: List[A])(f: A => B): List[B] = ma.map(f)
  })
```

# Usage

```tut
val l = List(1, 2, 3).flatMap(x => List(x + 5))
val k = List(2, 3, 4)

for {
  x <- l
  y <- k
} yield x * y
```

# Laws

A monad instance needs to satisfy the monad laws stating `pure` is a right and left
identity for `flatMap`, in addition to those defined by [`Applicative`](./Applicative.html)
and [`Bind`](./Bind.html):

```tut
  def bindRightIdentity[F[_], A, T](in: F[A])(assert: (F[A], F[A]) => T)(implicit F: Monad[F]) =
    assert(in, F.flatMap(in)(F.pure))

  def bindLeftIdentity[F[_], A, B, T](in: A)(f: A => F[B])(assert: (F[B], F[B]) => T)(implicit F: Monad[F]) =
    assert(f(in), F.flatMap(F.pure(in))(f))
```

That is to say; `pure` is an identity for `flatMap`,
in a similar way to how it's an identity for `ap`.
