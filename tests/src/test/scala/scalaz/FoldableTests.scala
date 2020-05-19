package scalaz

import std.AllInstances._
import syntax.foldable._
import syntax.equal._
import org.scalacheck.Prop.forAll
import org.scalacheck.{Arbitrary, Properties}

object FoldableTests {
  def anyIsLazy[F[_], A](implicit F: Foldable[F], arb: Arbitrary[F[A]]) = forAll { (fa: F[A]) =>
    var i = 0
    fa any { x =>
      i = i + 1
      true
    }
    val expected = if (fa.empty) 0 else 1
    i === expected
  }

  def allIsLazy[F[_], A](implicit F: Foldable[F], arb: Arbitrary[F[A]]) = forAll { (fa: F[A]) =>
    var i = 0
    fa all { x =>
      i = i + 1
      false
    }
    val expected = if (fa.empty) 0 else 1
    i === expected
  }

  def anyConsistent[F[_], A](f: A => Boolean)(implicit F: Foldable[F], fa: Arbitrary[F[A]]) =
    forAll { (fa: F[A]) =>
      F.any(fa)(f) === F.toList(fa).exists(f)
    }

  def allConsistent[F[_], A](f: A => Boolean)(implicit F: Foldable[F], fa: Arbitrary[F[A]]) =
    forAll { (fa: F[A]) =>
      F.all(fa)(f) === F.toList(fa).forall(f)
    }

  def anyAndAllLazy[F[_]](implicit fa: Arbitrary[F[Int]], F: Foldable[F]) = {
    val p = new Properties("foldable")
    p.property("consistent any") = anyConsistent[F, Int](_ > 0)
    p.property("consistent all") = allConsistent[F, Int](_ > 0)
    p.property("any is lazy") = anyIsLazy[F, Int]
    p.property("all is lazy") = allIsLazy[F, Int]
    p
  }
}
