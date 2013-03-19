package scalaz
package typelevel

import scalaz.std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalaCheckBinding._
import org.scalacheck.{Gen, Arbitrary}

class ProductTest extends Spec {

  import syntax.typelevel.all._

  // for testing purposes, natural equality is good enough
  implicit def HListEqual[L <: HList] = Equal.equalA[L]

  implicit def HNilArbitrary = Arbitrary(Gen.value(HNil))

  implicit def HConsArbitrary[H, T <: HList](implicit H: Arbitrary[H], T: Arbitrary[T]): Arbitrary[H :: T] =
    Applicative[Arbitrary].apply2(H, T)(_ :: _)



  implicit val nilApplicative = KTypeClass[Applicative].emptyProduct.instance
  implicit val nilTraverse = KTypeClass[Traverse].emptyProduct.instance

  checkAll("Empty product", applicative.laws[TCNil#Product])
  checkAll("Empty product", traverse.laws[TCNil#Product])


  implicit val consApplicative = KTypeClass[Applicative].product[List, TCNil](Applicative[List], nilApplicative)
  implicit val consTraverse = KTypeClass[Traverse].product[List, TCNil](Traverse[List], nilTraverse)

  checkAll("One-element product", applicative.laws[TCCons[List, TCNil]#Product])
  checkAll("One-element product", traverse.laws[TCCons[List, TCNil]#Product])


  implicit val consConsApplicative = KTypeClass[Applicative].product[Option, TCCons[List, TCNil]](Applicative[Option], consApplicative)
  implicit val consConsTraverse = KTypeClass[Traverse].product[Option, TCCons[List, TCNil]](Traverse[Option], consTraverse)

  checkAll("Two-element product", applicative.laws[TCCons[Option, TCCons[List, TCNil]]#Product])
  checkAll("Two-element product", traverse.laws[TCCons[Option, TCCons[List, TCNil]]#Product])

}

// vim: expandtab:ts=2:sw=2
