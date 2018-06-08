---
layout: docs
title:  "Forall"
---

# Forall [![GitHub](../img/github.png)](https://github.com/scalaz/scalaz/blob/series/8.0.x/base/shared/src/main/scala/scalaz/data/forall.scala)

Scala has polymorphic types (`class Foo[A]`, `type Bar[A]`, ...)
and polymorphic methods (`def foo[A]`), but it lacks polymorphic _values_.
As a special case, it lacks polymorphic functions (since functions are values).

A value polymorphic in type `A` is a _value_ that behaves uniformly over _all_ types `A`.
It is also called a _universally quantified_ value.

The advantage of polymorphic values over polymorphic methods is
that values, unlike methods, can be passed as arguments to other methods.

Scalaz encoding of polymorphic values also has the property that
_instantiating_ a polymorphic value at a specific type is a no-op at runtime.
The polymorphic value itself already is a value of the specific type as well.
All instantiations of a polymorphic value share the same runtime object.

Scalaz datatype `Forall[F[_]]` (also aliased as `∀[F[_]]`)
represents values polymorphic in one type variable.

Datatype `Forall2[F[_, _]]` (also aliased as `∀∀[F[_, _]]`)
represents values polymorphic in two type variables.

Polymorphic values come up naturally in many circumstances,
as is illustrated by the examples below.

**Typical imports**

```tut:silent
import scalaz._
```

# Polymorphic identity function

In Scala, we can define the polymorphic identity _method_:

```tut
def identity[A](a: A): A = a
```

But how would we define it as a _value_ (of a function type) that
works on any type? Let's start by writing down its type:

```tut
type IdFun[A] = A => A
type PolyIdFun = ∀[IdFun]
```

To create a value of `PolyIdFun`, we can use the `∀.mk` method:

```tut
val identity = ∀.mk[PolyIdFun].from(a => a)
```

The body of the implementation (here `a => a`) has to be typable
as `A => A` for any type `A`.

Expanding the type alias `PolyIdFun`, the above is equivalent to

```tut
val identity = ∀.mk[∀[IdFun]].from(a => a)
```

Alternatively, we can use the `∀.of` method and omit the `∀` in the type parameter:

```tut
val identity = ∀.of[IdFun](a => a)
```

With some help from [kind-projector](https://github.com/non/kind-projector/),
we can avoid intermediate type aliases altogether:

```tut
val identity = ∀.of[λ[A => A => A]](a => a)
```

To invoke this polymorphic function, we first _instantiate_ it at a specific type:

```tut
identity[Int]
```

and then apply it to an argument

```tut
identity[Int](5)
```


# Empty data structures

Consider a generic data structure, such as `List[A]` or `Option[A]`.
The empty case (like `Nil` or `None`) can be the same runtime object
for all types `A`.

The way this is achieved in the Scala standard library is to declare
such data structures covariant (`List[+A]`, `Option[+A]`, ...)
and have the empty case be an instantiation of the data structure
at the bottom type `Nothing` (`object Nil extends List[Nothing]`).

But covariance is not necessary (and often undesirable).
These empty values are in fact polymorphic.
We can define an invariant version of `Option` with the empty case
shared across all types:

<!--
tut does not support :paste mode,
which is required to define companion objects in the REPL.
See https://github.com/tpolecat/tut/issues/62.
-->
```scala
sealed trait IOption[A]
case class ISome[A](a: A) extends IOption[A]
case class INone[A] private() extends IOption[A]

object INone {
  private val instance = ∀.of[IOption](new INone())

  def apply[A](): IOption[A] = instance[A]
}
```

We can also create polymorphic versions of the standard empty structures:

```tut
val nil: ∀[List] = ∀.of[List](Nil)
val emptyMap: ∀∀[Map] = ∀∀.of[Map](Map())
```


# Universally quantified Semigroup

The typeclass

```scala
trait Plus[F[_]] {
  def plus[A](x: F[A], y: => F[A]): F[A]
}
```

is equivalently just a polymorphic semigroup:

```tut
type Plus[F[_]] = ∀[λ[A => Semigroup[F[A]]]]
```

Here is an instance for list:

```tut
import scalaz.algebra.SemigroupClass

def listSemigroup[A]: Semigroup[List[A]] = instanceOf(new SemigroupClass[List[A]] {
  def mappend(x: List[A], y: => List[A]) = x ++ y
})

val listPlus: Plus[List] = ∀.mk[Plus[List]].from(listSemigroup)

listPlus[Int].mappend(List(1, 2), List(3, 4))
```


# Natural transformation

Polymorphic functions are natural transformations.
They are a generalization of the polymorphic identity function above.

```tut
type ~>[F[_], G[_]] = ∀[λ[A => F[A] => G[A]]]

val headOption: List ~> Option = ∀.mk[List ~> Option].from(_.headOption)

headOption[Int](List(1, 2, 3))
```

The above `identity` function is a special case of natural transformation:

```tut
val identity = ∀.mk[Id ~> Id].from(a => a)

identity.apply(5)
```

We can also apply a polymorphic function to a polymorphic value and
obtain a polymorphic result.

```tut
implicit class NaturalTransformationOps[F[_], G[_]](trans: F ~> G) {
  def $(f: ∀[F]): ∀[G] = ∀.of[G](trans.apply.apply(f.apply))
}

headOption $ nil
```

## Binatural transformation

Function polymorphic in two type variables.

```tut
type ~~>[F[_, _], G[_, _]] = ∀∀[λ[(α, β) => F[α, β] => G[α, β]]]

type Option2[A, B] = Option[(A, B)]
val pick: Map ~~> Option2 = ∀∀.mk[Map ~~> Option2].from(_.headOption)

pick[String, Int](Map("hi" -> 5))

implicit class BinaturalTransformationOps[F[_, _], G[_, _]](trans: F ~~> G) {
  def $(f: ∀∀[F]): ∀∀[G] = ∀∀.of[G](trans.apply.apply(f.apply))
}

val none2: ∀∀[Option2] = pick $ emptyMap
```


# Restrictions

Since the same runtime object is shared among all instantiations
of a polymorphic value, it is not a good idea to create polymorphic
values of mutable data types. It even leads to unsoundness:

```tut
class Foo[A](var elems: List[A])

val foo = ∀.of[Foo](new Foo(Nil))

foo[Int].elems = List(1, 2, 3)
```

```tut:fail
val s: String = foo[String].elems.head
```
