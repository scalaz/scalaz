package scalaz
package typelevel

import scalaz.std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalaCheckBinding._
import org.scalacheck.Arbitrary

import Typelevel._

class ProductTest extends Spec {

  implicit val nilArbitrary = TypeClass[Arbitrary].emptyProduct
  implicit val nilEqual = TypeClass[Equal].emptyProduct

  implicit val nilApplicative = KTypeClass[ApplicativePlus].emptyProduct.instance
  implicit val nilTraverse = KTypeClass[Traverse].emptyProduct.instance

  checkAll("Empty product", applicative.laws[TCNil#Product])
  checkAll("Empty product", plusEmpty.laws[TCNil#Product])
  checkAll("Empty product", traverse.laws[TCNil#Product])


  implicit val consArbitraryV = implicitly[Arbitrary[List[Int]]] *: nilArbitrary
  implicit val consArbitraryF = implicitly[Arbitrary[List[Int => Int]]] *: nilArbitrary
  implicit val consEqual = Equal[List[Int]] *: nilEqual

  implicit val consApplicative = KTypeClass[ApplicativePlus].product[List, TCNil](ApplicativePlus[List], nilApplicative)
  implicit val consTraverse = KTypeClass[Traverse].product[List, TCNil](Traverse[List], nilTraverse)

  checkAll("One-element product", applicative.laws[TCCons[List, TCNil]#Product])
  checkAll("One-element product", plusEmpty.laws[TCCons[List, TCNil]#Product])
  checkAll("One-element product", traverse.laws[TCCons[List, TCNil]#Product])


  implicit val consConsArbitraryV = implicitly[Arbitrary[Option[Int]]] *: consArbitraryV
  implicit val consConsArbitraryF = implicitly[Arbitrary[Option[Int => Int]]] *: consArbitraryF
  implicit val consConsEqual = Equal[Option[Int]] *: consEqual

  implicit val consConsApplicative = KTypeClass[ApplicativePlus].product[Option, TCCons[List, TCNil]](ApplicativePlus[Option], consApplicative)
  implicit val consConsTraverse = KTypeClass[Traverse].product[Option, TCCons[List, TCNil]](Traverse[Option], consTraverse)

  checkAll("Two-element product", applicative.laws[TCCons[Option, TCCons[List, TCNil]]#Product])
  checkAll("Two-element product", plusEmpty.laws[TCCons[Option, TCCons[List, TCNil]]#Product])
  checkAll("Two-element product", traverse.laws[TCCons[Option, TCCons[List, TCNil]]#Product])

}

// vim: expandtab:ts=2:sw=2
