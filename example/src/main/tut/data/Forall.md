---
layout: docs
title:  "Forall"
---

# Forall

## Instance creation

Create instances using `of` or `mk`

```tut
import scalaz.Prelude._

val nil: ∀[List] = ∀.of[List](Nil)
val emptyMap: ∀∀[Map] = ∀∀.of[Map](Map())

val nil1: ∀[List] = ∀.mk[∀[List]].from(Nil)
val emptyMap1: ∀∀[Map] = ∀∀.mk[∀∀[Map]].from(Map())
```

## Universally quantified Semigroup

```tut
import scalaz.Prelude._
import scalaz.typeclass.Semigroup

def listSemigroup[A]: Semigroup[List[A]] = new Semigroup[List[A]] {
  def append(x: List[A], y: => List[A]) = x ++ y
}

type Plus[F[_]] = ∀[λ[A => Semigroup[F[A]]]]

val listPlus: Plus[List] = ∀.mk[Plus[List]].from(listSemigroup)
listPlus[Int].append(List(1, 2), List(3, 4)) == List(1, 2, 3, 4)
```

## Natural transformation

```tut
import scalaz.Prelude._

type ~>[F[_], G[_]] = ∀[λ[A => F[A] => G[A]]]

val headOption: List ~> Option = ∀.mk[List ~> Option].from(_.headOption)

headOption[Int](List(1, 2, 3)) == Some(1)

implicit class NaturalTransformationOps[F[_], G[_]](trans: F ~> G) {
  def $(f: ∀[F]): ∀[G] = ∀.of[G](trans.apply.apply(f.apply))
}

val none: ∀[Option] = headOption $ nil
```

## Binatural transformation

```tut
type ~~>[F[_, _], G[_, _]] = ∀∀[λ[(α, β) => F[α, β] => G[α, β]]]

type Option2[A, B] = Option[(A, B)]
val pick: Map ~~> Option2 = ∀∀.mk[Map ~~> Option2].from(_.headOption)

pick[String, Int](Map("hi" -> 5)) == Some("hi" -> 5)

implicit class BinaturalTransformationOps[F[_, _], G[_, _]](trans: F ~~> G) {
  def $(f: ∀∀[F]): ∀∀[G] = ∀∀.of[G](trans.apply.apply(f.apply))
}

val none2: ∀∀[Option2] = pick $ emptyMap
```
