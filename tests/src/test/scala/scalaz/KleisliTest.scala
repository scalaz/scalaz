package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.{Gen, Arbitrary}

class KleisliTest extends Spec {

  type KleisliOpt[A, B] = Kleisli[Option, A, B]

  implicit def Function1IntOptInt[A](implicit A: Arbitrary[Option[Int]]): Arbitrary[Int => Option[Int]] =
    Arbitrary(Gen.frequency[Int => Option[Int]](
      (1, Gen.value((x: Int) => Some(x))),
      (1, Gen.value((x: Int) => Some(x + 1))),
      (3, A.arbitrary.map(a => (_: Int) => a))
    ))

  implicit def KleisliEqual[M[_]](implicit M: Equal[M[Int]]): Equal[Kleisli[M, Int, Int]] = new Equal[Kleisli[M, Int, Int]] {
    def equal(a1: Kleisli[M, Int, Int], a2: Kleisli[M, Int, Int]): Boolean = {
      val mb1: M[Int] = a1.run(0)
      val mb2: M[Int] = a2.run(0)
      M.equal(mb1, mb2)
    }
  }

  checkAll(category.laws[KleisliOpt])
}