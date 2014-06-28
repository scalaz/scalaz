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
      (F maximum xs: Option[Int]) must_===(Tag unsubst (F minimum xsdual: Option[Int @@ Tags.Dual]))
      (F minimum xs: Option[Int]) must_===(Tag unsubst (F maximum xsdual: Option[Int @@ Tags.Dual]))
  }

  "semigroups min" ! forAll {
    (xs: NonEmptyList[Int]) =>
      val F = Foldable1[NonEmptyList]
      import Tags._
      import syntax.foldable1._
      Tag unwrap (MinVal.subst(xs).suml1) must_===(F minimum1 xs)
  }

  "semigroups max" ! forAll {
    (xs: NonEmptyList[Int]) =>
      val F = Foldable1[NonEmptyList]
      import Tags._
      import syntax.foldable1._
      Tag unwrap (MaxVal.subst(xs).suml1) must_===(F maximum1 xs)
  }
}
