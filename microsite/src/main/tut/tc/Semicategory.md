---
layout: scalaz
module: base
section: typeclass
source: tc/semicategory.scala
title:  "Semicategory"
---

A `Semicategory` is a binary type constructor with an associative binary
operation `compose`. It is a [`Category`](./Category.html) that may not have
an identity element `F[A, A]` for every `A`.

# Typical imports

```tut:silent
import scalaz.tc.Semicategory
```

In the literature and in Haskell, this is sometimes known as a `Semigroupoid`, by analogy
to "groupoids" (categories where all morphisms are isomorphisms) and
"monoidoids" (categories).

The "semi-" prefix usually means "without identity";
the identity for a `Category` is `id`.

# Law

The sole law for a valid `Semicategory` is that composition is associative:

```tut
def composeAssoc[F[_, _], A, B, C, D, T](fst: F[C, D], snd: F[B, C], thd: F[A, B])(
  assert: (F[A, D], F[A, D]) => T
)(implicit F: Semicategory[F]): T = {
  import F.compose
  assert(
    compose(compose(fst, snd), thd),
    compose(fst, compose(snd, thd))
  )
}
```
