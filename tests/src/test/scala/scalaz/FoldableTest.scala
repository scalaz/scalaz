package scalaz

import std.anyVal._
import syntax.foldable._
import syntax.equal._
import org.scalacheck.Prop.forAll
import org.scalacheck.Arbitrary

object FoldableTests {
  def anyIsLazy[F[_], A](implicit F: Foldable[F], arb: Arbitrary[F[A]]) = forAll { fa: F[A] =>
    var i = 0
    fa any { x =>
      i = i + 1
      true
    }
    val expected = if (fa.empty) 0 else 1
    i === expected
  }
}
