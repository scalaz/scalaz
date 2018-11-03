---
layout: scalaz
module: base
section: typeclass
source: tc/bifunctor.scala
title:  "Bifunctor"
---

*The `Bifunctor` type class describes a type which has two type parameters,
 each of which acts as a (covariant) [Functor](./Functor.html).*

## Instance declaration

A `Bifunctor` can be declared by specifying either `bimap`, or both `lmap` and `rmap`.

```tut
import scala.Tuple2
import scalaz._, tc._, data.{\&/, These}
import Scalaz._

val tuple2Bifunctor: Bifunctor[Tuple2] =
  instanceOf(new BifunctorClass[Tuple2] {
    override def lmap[A, B, S](fab: (A, B))(f: A => S): (S, B) = fab.copy(_1 = f(fab._1))
    override def rmap[A, B, T](fab: (A, B))(f: B => T): (A, T) = fab.copy(_2 = f(fab._2))
  })

val theseBifunctor: Bifunctor[These] =
  instanceOf(new BifunctorClass[These] {
    override def bimap[A, B, S, T](fab: A \&/ B)(as: A => S, bt: B => T): S \&/ T =
      fab.bimap(as)(bt)
  })
```

# Law

A `Bifunctor` must satisfy the following law:

```tut
def identityToIdentity[F[_, _], A, B, T](
  in: F[A, B]
)(assert: (F[A, B], F[A, B]) => T)(implicit F: Bifunctor[F]): T =
  assert(in, F.bimap(in)((a: A) => a, (b: B) => b))
```

This is analogous to the ordinary `Functor` law,
which states that `Functor`s take `identity: A => B`
to `identity: F[A] => F[B]`, but the identity in this case
is a pair of `identity` functions and not a single `identity` function.

It serves the same purpose as for ordinary `Functor`'s;
`bimap` shouldn't modify the `F[_]` context.

Here's a free law, implied by that law and the type of `bimap`:

```tut
def freeComposition[F[_, _], A, B, C, X, Y, Z, T](
  in: F[A, X]
)(f: A => B, g: B => C
)(h: X => Y, i: Y => Z
)(assert: (F[C, Z], F[C, Z]) => T
)(implicit F: Bifunctor[F]): T =
  assert(
    F.bimap(in)(f.andThen(g), h.andThen(i)),
    F.bimap(F.bimap(in)(f, h))(g, i)
  )
```

This states that bifunctors respect function composition,
but again, because it's free, it's not necessarily useful to test it.
