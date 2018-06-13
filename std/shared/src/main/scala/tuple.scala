package scalaz
package std

import scala.{ Tuple1, Tuple2 }

import scalaz.core.EqAnyRef

trait TupleInstances {
  implicit final def tuple1Eq[A](implicit A: Eq[A]): Eq[Tuple1[A]] =
    instanceOf({
      case (Tuple1(a1), Tuple1(a2)) => A.equal(a1, a2)
      case _                        => false
    }: EqAnyRef[Tuple1[A]])

  implicit final def tuple2Eq[A, B](implicit A: Eq[A], B: Eq[B]): Eq[Tuple2[A, B]] =
    instanceOf({
      case ((a1, b1), (a2, b2)) => A.equal(a1, a2) && B.equal(b1, b2)
      case _                    => false
    }: EqAnyRef[Tuple2[A, B]])

  implicit final def tuple3Eq[A, B, C](implicit A: Eq[A], B: Eq[B], C: Eq[C]): Eq[(A, B, C)] =
    instanceOf({
      case ((a1, b1, c1), (a2, b2, c2)) =>
        A.equal(a1, a2) && B.equal(b1, b2) && C.equal(c1, c2)
      case _ => false
    }: EqAnyRef[(A, B, C)])

  implicit final def tuple4Eq[A, B, C, D](implicit A: Eq[A], B: Eq[B], C: Eq[C], D: Eq[D]): Eq[(A, B, C, D)] =
    instanceOf({
      case ((a1, b1, c1, d1), (a2, b2, c2, d2)) =>
        A.equal(a1, a2) && B.equal(b1, b2) && C.equal(c1, c2) && D.equal(d1, d2)
      case _ => false
    }: EqAnyRef[(A, B, C, D)])
}
