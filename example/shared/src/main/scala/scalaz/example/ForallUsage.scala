package scalaz
package example

import scalaz.data.∀
import scalaz.data.forall._

object ForallUsage extends App {
  import scalaz.typeclass.Semigroup

  def listSemigroup[A]: Semigroup[List[A]] = new Semigroup[List[A]] {
    def append(x: List[A], y: => List[A]) = x ++ y
  }

  // isntances can be created using ∀.of syntax
  val nil: ∀[List] = ∀.of[List](Nil)

  // or ∀.mk syntax
  val nil1: ∀[List] = ∀.mk[∀[List]].from(Nil)


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

  val none: ∀[Option] = headOption $ nil
}
