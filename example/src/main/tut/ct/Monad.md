---
layout: docs
title:  "Monad"
---

# Monad

*A monad is the combination of an Applicative with a Bind*

A monad instance needs to satisfy the monad laws in addition to those defined by [Applicative](./Applicative.html) and [Bind](./Bind.html):

- Left identity: `pure(a).flatMap(f) === f(a)`
- Right identity: `x.flatMap(pure) === x`
- Associativity: `x.flatMap(f).flatMap(g) === m.flatMap(x => f(x).flatMap(g))`

**Typical imports**

```tut:silent
import scalaz.Scalaz._
```

## Instance declaration

```tut
import scalaz.Prelude._
import scalaz.ct.MonadClass

val listMonad: Monad[List] = instanceOf(new MonadClass[List] {
  def pure[A](a: A): List[A] = List(a)
  def ap[A, B](fa: List[A])(f: List[A => B]): List[B] = fa.zip(f).map(t => t._2(t._1))
  def flatMap[A, B](ma: List[A])(f: A => List[B]): List[B] = ma.flatMap(f)
  def flatten[A](ma: List[List[A]]): List[A] = ma.flatten
  def map[A, B](ma: List[A])(f: A => B): List[B] = ma.map(f)
})
```

## Usage

```tut
val l = List(1, 2, 3).flatMap(x => List(x + 5))
val k = List(2, 3, 4)

for {
  x <- l
  y <- k
} yield x * y
```
