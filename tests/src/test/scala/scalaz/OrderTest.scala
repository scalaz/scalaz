package scalaz

import std.AllInstances._
import Dual._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.Prop.forAll

object OrderTest extends SpecLite {
  "duals" ! forAll {
    (xs: List[Int]) =>
      val F = Foldable[List]
      val xsdual: List[Int @@ Tags.Dual] = Tag subst xs
      (F maximum xs: Option[Int]) must_===(F minimum xsdual: Option[Int])
      (F minimum xs: Option[Int]) must_===(F maximum xsdual: Option[Int])
  }

  "semigroups min" ! forAll {
    (xs: NonEmptyList[Int]) =>
      val F = Foldable1[NonEmptyList]
      import Tags._
      import syntax.foldable1._
      ((xs map MinVal).suml1: Int) must_===(F minimum1 xs)
  }

  "semigroups max" ! forAll {
    (xs: NonEmptyList[Int]) =>
      val F = Foldable1[NonEmptyList]
      import Tags._
      import syntax.foldable1._
      ((xs map MaxVal).suml1: Int) must_===(F maximum1 xs)
  }
}
