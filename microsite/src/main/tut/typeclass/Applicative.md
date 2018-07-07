---
layout: docs
title:  "Applicative"
---

# Applicative [![GitHub](../img/github.png)](https://github.com/scalaz/scalaz/blob/series/8.0.x/base/shared/src/main/scala/scalaz/tc/applicative.scala)

*Whereas a [functor](./Functor.html) allows application of a pure function to a value in a context, an Applicative also allows application of a function in a context to a value in a context.*

**Typical imports**

```tut:silent
import scalaz.tc._
import scalaz.Predef._
import scalaz.Scalaz._
```

# Instance declaration

```tut
case class ZipList[A](val value: List[A]) extends AnyVal

/* Note that this is _not_ the Applicative instance for List! */
implicit val zipListAp: Applicative[ZipList] = instanceOf(new ApplicativeClass[ZipList] {
  def pure[A](a: A): ZipList[A] = ZipList(List(a))

  def ap[A, B](fa: ZipList[A])(f: ZipList[A => B]): ZipList[B] =
    ZipList((fa.value zip f.value).map(t => t._2(t._1)))

  def map[A, B](fa: ZipList[A])(f: A => B): ZipList[B] =
    ZipList(fa.value map f)
})
```

# Usage

```tut
val l: ZipList[Int] = 123.pure[ZipList]

l.map(_ + 25)

val l2 = ZipList(List(17, 19, 23))

val fs: ZipList[Int => Int] = ZipList(List(_ * 2, _ * 3, _ * 4, _ * 5))

l2.ap(fs)
```
