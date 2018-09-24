---
layout: docs
section: typeclass
title:  "Bind"
---

# Bind [![GitHub](../img/github.png)](https://github.com/scalaz/scalaz/blob/series/8.0.x/base/shared/src/main/scala/scalaz/tc/bind.scala)

Equivalent to [Monad](./Monad.html) without the `pure` method.

A semi-monad.

The "semi-" prefix usually means "without identity";
the identity for a `Monad` is `pure`.

`Bind` relates to `Semigroup` as `Monad` relates to `Monoid`.

This statement can be made exact by recognizing the category in which `Monad`
is a `Monoid`; `Bind` is a semigroup in that category.

**Typical imports**

```tut:silent
import scalaz.tc._
import scalaz.Predef._
import scalaz.Scalaz._
```

# Law

The only law introduced in addition to the `Apply` laws is:

```tut
def bindAssoc[F[_], A, B, C, T](in: F[A])(fst: A => F[B],
                                          snd: B => F[C])(assert: (F[C], F[C]) => T)(implicit F: Bind[F]): T = {
  import F.flatMap
  assert(
    flatMap(flatMap(in)(fst))(snd),
    flatMap(in)(b => flatMap(fst(b))(snd))
  )
}
```

That is to say, `flatMap` is associative composition of Kleisli arrows in `F[_]`,
where `type Kleisli[F[_], A, B] = A => F[B]`.
