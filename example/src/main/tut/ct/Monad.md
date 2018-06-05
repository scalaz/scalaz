---
layout: docs
title:  "Monad"
---

# Monad [![GitHub](../img/github.png)](https://github.com/scalaz/scalaz/blob/series/8.0.x/base/shared/src/main/scala/scalaz/ct/monad.scala)

*A monad is the combination of an `Applicative` and a `Bind`.*

A monad instance needs to satisfy the monad laws in addition to those defined by
[`Applicative`](./Applicative.html) and [`Bind`](./Bind.html):

- Left identity: `pure(a).flatMap(f) === f(a)`
- Right identity: `x.flatMap(pure) === x`
- Associativity: `x.flatMap(f).flatMap(g) === m.flatMap(x => f(x).flatMap(g))`

In addition, it must "play nicely" with the `Applicative` instance:

- `fa.ap(ff) === fa.flatMap(a => ff.map(f => f(a)))`

**Typical imports**

```tut:silent
import scalaz.Scalaz._
```

# Instance declaration

```tut
import scalaz.Prelude._
import scalaz.ct.{BindClass, MonadClass}

val listMonad: Monad[List] = 
  instanceOf(new MonadClass[List] with BindClass.DeriveAp[List] {
    def pure[A](a: A): List[A] = List(a)
    def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)
    def flatten[A](ma: List[List[A]]): List[A] = ma.flatten
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
