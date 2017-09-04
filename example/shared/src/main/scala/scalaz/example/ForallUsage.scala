package scalaz
package example

import Prelude._

object ForallUsage extends App {
  import scalaz.typeclass.Semigroup

  def listSemigroup[A]: Semigroup[List[A]] = new Semigroup[List[A]] {
    def append(x: List[A], y: => List[A]) = x ++ y
  }

  // isntances can be created using `of` syntax
  val nil: ∀[List] = ∀.of[List](Nil)
  val emptyMap: ∀∀[Map] = ∀∀.of[Map](Map())

  // or `mk` syntax
  val nil1: ∀[List] = ∀.mk[∀[List]].from(Nil)
  val emptyMap1: ∀∀[Map] = ∀∀.mk[∀∀[Map]].from(Map())


  /* universally quantified semigroup */

  type Plus[F[_]] = ∀[λ[A => Semigroup[F[A]]]]

  // create an instance
  val listPlus: Plus[List] =
    ∀.mk[Plus[List]].from(listSemigroup)

  // use the instance
  assert( listPlus[Int].append(List(1, 2), List(3, 4)) == List(1, 2, 3, 4) )


  /* natural transformation */

  type ~>[F[_], G[_]] = ∀[λ[A => F[A] => G[A]]]

  // create an instance
  val headOption: List ~> Option = ∀.mk[List ~> Option].from(_.headOption)

  // use the instance
  assert( headOption[Int](List(1, 2, 3)) == Some(1) )

  // extra syntax for applying a natural transformation to universally quantified value
  implicit class NaturalTransformationOps[F[_], G[_]](trans: F ~> G) {
    def $(f: ∀[F]): ∀[G] = ∀.of[G](trans.apply.apply(f.apply))
  }

  // applying a universally quantified function to a universally quantified value
  // yields a universally quantified value
  val none: ∀[Option] = headOption $ nil


  /* binatural transformation */
  type ~~>[F[_, _], G[_, _]] = ∀∀[λ[(α, β) => F[α, β] => G[α, β]]]

  // create an instance
  type Option2[A, B] = Option[(A, B)]
  val pick: Map ~~> Option2 = ∀∀.mk[Map ~~> Option2].from(_.headOption)

  // use the instance
  assert( pick[String, Int](Map("hi" -> 5)) == Some("hi" -> 5) )

  // extra syntax for applying a binatural transformation to univarsally quantified value
  implicit class BinaturalTransformationOps[F[_, _], G[_, _]](trans: F ~~> G) {
    def $(f: ∀∀[F]): ∀∀[G] = ∀∀.of[G](trans.apply.apply(f.apply))
  }

  // applying a universally quantified function to a universally quantified value
  // yields a universally quantified value
  val none2: ∀∀[Option2] = pick $ emptyMap
}
