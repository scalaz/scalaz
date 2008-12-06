package fj.data

import pre.Ord
import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import fj.data.Set.{empty}

object ArbitrarySet {
  implicit def arbitrarySet[A](implicit a: Arbitrary[A], o:Ord[A]): Arbitrary[Set[A]] =
    Arbitrary(setOf(arbitrary[A], o))

  def setOf[A](g : => Gen[A], o : Ord[A]) : Gen[Set[A]] =
    Gen.listOf(g).map(_.foldLeft(empty(o))((s:Set[A], a:A) => s.insert(a)))
}
