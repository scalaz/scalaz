package fj.data

import org.scalacheck.Gen
import org.scalacheck.Arbitrary
import org.scalacheck.Arbitrary.arbitrary
import List.{nil, cons}

object ArbitraryList {
  implicit def arbitraryList[A](implicit a: Arbitrary[A]): Arbitrary[List[A]] =
    Arbitrary(listOf(arbitrary[A]))

  def listOf[A](g : => Gen[A]) : Gen[List[A]] =
    Gen.listOf(g).map(_.foldRight(nil[A])(cons(_, _)))
}
