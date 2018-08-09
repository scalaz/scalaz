---
layout: docs
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
  instanceOf(new MonadClass[List] with BindClass.DeriveAp[List] {
    def pure[A](a: A): List[A] = List(a)
    def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)
    def flatten[A](ma: List[List[A]]): List[A] = ma.flatMap(x => x)
    def map[A, B](ma: List[A])(f: A => B): List[B] = ma.map(f)
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

# Law

A monad instance needs to satisfy the monad law stating `pure` is a right
identity for `flatMap`, in addition to those defined by
[`Applicative`](./Applicative.html) and [`Bind`](./Bind.html):

```tut
  def bindIdentity[F[_], A, T](in: F[A])(assert: (F[A], F[A]) => T)(implicit F: Monad[F]) =
    assert(in, F.flatMap(in)(F.pure))
```

That is to say; `pure` is an identity for `flatMap`,
in a similar way to how it's an identity for `ap`.

The "left identity law" is guaranteed by parametricity and the right identity law:
```tut
  def freeLeftBindIdentity[F[_], A, B, T](in: A)(f: A => F[B])(assert: (F[B], F[B]) => T)(implicit F: Monad[F]): T =
    assert(f(in), F.flatMap(F.pure(in))(f))
```
