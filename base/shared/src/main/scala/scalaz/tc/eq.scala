package scalaz
package tc

import Predef._
import data.Void

import java.lang.Double.doubleToRawLongBits
import java.lang.Float.floatToRawIntBits
import scala.collection.immutable.Set
import scala.{ Either, Left, List, None, Option, Right, Some, Tuple1, Vector }

import scala.language.experimental.macros

trait EqClass[A] {
  def equal(first: A, second: A): Boolean
}

object EqClass {
  implicit def eitherEq[L, R](implicit X: Eq[L], Y: Eq[R]): Eq[Either[L, R]] =
    instanceOf(new EqClass[Either[L, R]] {
      def equal(first: Either[L, R], second: Either[L, R]): Boolean = (first, second) match {
        case (Left(x), Left(y))   => X.equal(x, y)
        case (Right(x), Right(y)) => Y.equal(x, y)
        case _                    => false
      }
    })

  implicit def listEq[A: Eq]: Eq[List[A]] =
    instanceOf(new EqClass[List[A]] {
      def equal(first: List[A], second: List[A]): Boolean = (first.corresponds(second)(Eq[A].equal))
    })

  implicit def optionEq[A](implicit X: Eq[A]): Eq[Option[A]] =
    instanceOf(new EqClass[Option[A]] {
      def equal(first: Option[A], second: Option[A]): Boolean = (first, second) match {
        case (None, None)       => true
        case (Some(a), Some(b)) => X.equal(a, b)
        case _                  => false
      }
    })

  implicit def setEq[A: Eq]: Eq[Set[A]] =
    instanceOf(new EqClass[Set[A]] {
      def equal(first: Set[A], second: Set[A]): Boolean = (first.toStream.corresponds(second.toStream)(Eq[A].equal))
    })

  implicit def vectorEq[A: Eq]: Eq[Vector[A]] =
    instanceOf(new EqClass[Vector[A]] {
      def equal(first: Vector[A], second: Vector[A]): Boolean = (first.corresponds(second)(Eq[A].equal))
    })

  implicit final def tuple1Eq[A](implicit A: Eq[A]): Eq[Tuple1[A]] =
    instanceOf[EqClass[Tuple1[A]]] {
      case (Tuple1(a1), Tuple1(a2)) => A.equal(a1, a2)
      case _                        => false
    }

  implicit final def tuple2Eq[A, B](implicit A: Eq[A], B: Eq[B]): Eq[(A, B)] =
    instanceOf[EqClass[(A, B)]] {
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

  private[this] final val NegZero: Double = -0.0d
  private[this] final val PosZero: Double = 0.0d

  implicit val intEq: Eq[Int]         = Eq.fromEquals[Int]
  implicit val longEq: Eq[Long]       = Eq.fromEquals[Long]
  implicit val stringEq: Eq[String]   = Eq.fromEquals[String]
  implicit val unitEq: Eq[Unit]       = Eq.always[Unit]
  implicit val shortEq: Eq[Short]     = Eq.fromEquals[Short]
  implicit val floatEq: Eq[Float]     = instanceOf[EqClass[Float]]((a, b) => floatToRawIntBits(a) == floatToRawIntBits(b))
  implicit val byteEq: Eq[Byte]       = Eq.fromEquals[Byte]
  implicit val booleanEq: Eq[Boolean] = Eq.fromEquals[Boolean]
  implicit val doubleEq: Eq[Double] =
    instanceOf(new EqClass[Double] {
      def equal(a: Double, b: Double) = (a, b) match {
        case (NegZero, PosZero) => true
        case (PosZero, NegZero) => true
        case (x, y)             => doubleToRawLongBits(x) == doubleToRawLongBits(y)
      }
    })
  implicit final val voidEq: Eq[Void] = instanceOf[EqClass[Void]]((a, b) => a.absurd)

}

trait EqSyntax {
  implicit final class ToEqOps[A](a: A) {
    private[tc] type equal
    def ===(f: A)(implicit ev: Eq[A]): Boolean = macro meta.Ops.nia_1[equal]
  }
}

object Eq {
  def always[A]: Eq[A]                   = instanceOf[EqClass[A]]((_, _) => true)
  def never[A]: Eq[A]                    = instanceOf[EqClass[A]]((_, _) => false)
  def fromEquals[A]: Eq[A]               = instanceOf[EqClass[A]](_ == _)
  def apply[A](implicit P: Eq[A]): Eq[A] = P
}
