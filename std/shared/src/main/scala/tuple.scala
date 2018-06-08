package scalaz
package std

import scala.{ Tuple1, Tuple2 }

import scalaz.core.EqClass

trait TupleInstances {
  implicit final def tuple1Eq[A](implicit A: Eq[A]): Eq[Tuple1[A]] =
    instanceOf[EqClass[Tuple1[A]]] {
      case (Tuple1(a1), Tuple1(a2)) => A.equal(a1, a2)
      case _                        => false
    }

  implicit final def tuple2Eq[A, B](implicit A: Eq[A], B: Eq[B]): Eq[Tuple2[A, B]] =
    instanceOf[EqClass[Tuple2[A, B]]] {
      case ((a1, b1), (a2, b2)) => A.equal(a1, a2) && B.equal(b1, b2)
      case _                    => false
    }

  implicit final def tuple3Eq[A, B, C](implicit A: Eq[A], B: Eq[B], C: Eq[C]): Eq[(A, B, C)] =
    instanceOf[EqClass[(A, B, C)]] {
      case ((a1, b1, c1), (a2, b2, c2)) =>
        A.equal(a1, a2) && B.equal(b1, b2) && C.equal(c1, c2)
      case _ => false
    }

  implicit final def tuple4Eq[A, B, C, D](implicit A: Eq[A], B: Eq[B], C: Eq[C], D: Eq[D]): Eq[(A, B, C, D)] =
    instanceOf[EqClass[(A, B, C, D)]] {
      case ((a1, b1, c1, d1), (a2, b2, c2, d2)) =>
        A.equal(a1, a2) && B.equal(b1, b2) && C.equal(c1, c2) && D.equal(d1, d2)
      case _ => false
    }
}
