---
layout: docs
title:  "Applicative"
---

# Applicative

*Whereas a [functor](./Functor.html) allows application of a pure function to a value in a context, an Applicative also allows application of a function in a context to a value in a context.*

**Typical imports**

```tut:silent
import scalaz.Prelude._
```

## Instance declaration

```tut
import scalaz.typeclass.ApplicativeClass

implicit val listap: Applicative[List] = new ApplicativeClass[List] {
  def pure[A](a: A): List[A] = List(a)
  def ap[A, B](fa: List[A])(f: List[A => B]): List[B] = fa.zip(f).map(t => t._2(t._1))
  def map[A, B](ma: List[A])(f: A => B): List[B] = ma.map(f)
}
```

## Usage

```tut

val l: List[Int] = 123.pure[List]

implicit val f: Functor[List] = listap.apply.functor

l.map(_ + 25)
```
