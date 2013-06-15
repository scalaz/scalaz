package scalaz

import std.AllInstances._
import Dual._
import scalaz.scalacheck.ScalazArbitrary._

class OrderTest extends Spec {
  "duals" ! prop {
    (xs: List[Int]) =>
      val F = Foldable[List]
      val xsdual: List[Int @@ Tags.Dual] = Tag subst xs
      (F maximum xs: Option[Int]) must be_===(F minimum xsdual: Option[Int])
      (F minimum xs: Option[Int]) must be_===(F maximum xsdual: Option[Int])
  }

  "semigroups min" ! prop {
    (xs: NonEmptyList[Int]) =>
      val F = Foldable1[NonEmptyList]
      import Tags._
      import syntax.foldable1._
      (xs map MinVal).suml1 must be_===(F minimum1 xs)
  }

  "semigroups max" ! prop {
    (xs: NonEmptyList[Int]) =>
      val F = Foldable1[NonEmptyList]
      import Tags._
      import syntax.foldable1._
      (xs map MaxVal).suml1 must be_===(F maximum1 xs)
  }
}
