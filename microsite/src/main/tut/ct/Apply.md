---
layout: docs
section: typeclass
title:  "Apply"
---

# Apply [![GitHub](../img/github.png)](https://github.com/scalaz/scalaz/blob/series/8.0.x/base/shared/src/main/scala/scalaz/ct/apply.scala)

*A strong lax semi-monoidal endofunctor. This is equivalent to an [Applicative](./Applicative.html) without pure.*

In addition to the laws of a [Functor](./Functor.html), an Apply instance needs to satisfy the following law:

- Associativity: `Apply.ap(Apply.ap(x)(fs1))(fs2) === Apply.ap(x)(Apply.ap(fs1)(fs2))`

**Typical imports**

```tut:silent
import scalaz.Scalaz._
```

# Instance declaration

```tut
import scalaz.ct.ApplyClass

val listApply: Apply[List] = instanceOf(new ApplyClass[List] {
  def ap[A, B](fa: List[A])(f: List[A => B]): List[B] = fa.zip(f).map(t => t._2(t._1))
  def map[A, B](ma: List[A])(f: A => B): List[B] = ma.map(f)
})
```

# Usage

```tut
val f1: Int => Int = _ + 3
val f2: Int => Int = _ - 2

List(1, 2, 3).ap(List(f1, f2, f1))
```
