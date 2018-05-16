---
layout: docs
title:  "Applicative"
---

# Applicative

*Whereas a [functor](./Functor.html) allows application of a pure function to a value in a context, an Applicative also allows application of a function in a context to a value in a context.*

**Typical imports**

```tut:silent
import scalaz.Scalaz._
```

## Instance declaration

```tut
import scalaz.Prelude._
import scalaz.ct.ApplicativeClass

implicit val listap: Applicative[List] = instanceOf(new ApplicativeClass[List] {
  def pure[A](a: A): List[A] = List(a)
  def ap[A, B](fa: List[A])(f: List[A => B]): List[B] = fa.zip(f).map(t => t._2(t._1))
  def map[A, B](ma: List[A])(f: A => B): List[B] = ma.map(f)
})
```

## Usage

```tut:reset
import scalaz.Scalaz._

val l: List[Int] = 123.pure[List]

l.map(_ + 25)

val l2 = List(17, 19, 23)

val fs: List[Int => Int] = List(_ * 2, _ * 3, _ * 4, _ * 5)

l2.ap(fs)
```

