---
layout: docs
title:  "Apply"
---

# Apply [![GitHub](../img/github.png)](https://github.com/scalaz/scalaz/blob/series/8.0.x/base/shared/src/main/scala/scalaz/tc/apply.scala)

Equivalent to [Applicative](./Applicative.html) without the `pure` method.

A strong lax semi-monoidal endofunctor.

The "semi-" prefix usually means "without identity";
the identity for an `Applicative` (a strong lax monoidal endofunctor)
is `pure`.

`Apply` relates to `Semigroup` as `Applicative` relates to `Monoid`.

This statement can be made exact by recognizing the category in which `Applicative`
is a `Monoid`; `Apply` is a semigroup in that category.

**Typical imports**

```tut:silent
import scalaz.Scalaz._
```

# Instance declaration

```tut
import scalaz.Predef._
import scalaz.tc._

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

# Law

The only law introduced in addition to the `Functor` laws is:

```tut
  def applyAssoc[F[_], A, B, C, T](in: F[A])(fst: F[A => B],
                                             snd: F[B => C])(assert: (F[C], F[C]) => T)(implicit F: Apply[F]): T = {
    import F.{ ap, map }
    def compose(f: B => C)(g: A => B): A => C = f compose g
    assert(
      ap(ap(in)(fst))(snd),
      ap(in)(ap(fst)(map(snd)(compose)))
    )
  }
```

That is to say, `ap` is associative composition of static arrows in
`F[_]`, where `type Static[F[_], A, B] = F[A => B]`.
