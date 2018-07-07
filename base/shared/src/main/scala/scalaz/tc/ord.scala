package scalaz
package tc

import java.lang.Double.doubleToRawLongBits
import scala.language.experimental.macros

import Predef._
import data.{ EQ, GT, LT, Ordering }

trait OrdClass[A] extends EqClass[A] {
  def comp(a: A, b: A): Ordering

  def <(a: A, b: A): Boolean  = comp(a, b) eq LT
  def <=(a: A, b: A): Boolean = comp(a, b) ne GT
  def >(a: A, b: A): Boolean  = comp(a, b) eq GT
  def >=(a: A, b: A): Boolean = comp(a, b) ne LT
  def equal(a: A, b: A)       = comp(a, b) eq EQ
}

object OrdClass {
  implicit val booleanOrd: Ord[Boolean] = instanceOf(new OrdClass[Boolean] {
    def comp(a: Boolean, b: Boolean) = (a, b) match {
      case (true, false) => GT
      case (false, true) => LT
      case _             => EQ
    }
  })
  implicit val byteOrd: Ord[Byte] = instanceOf(new OrdClass[Byte] {
    def comp(a: Byte, b: Byte) = java.lang.Byte.compare(a, b) match {
      case 0          => EQ
      case x if x < 0 => LT
      case _          => GT
    }
  })
  implicit val doubleOrd: Ord[Double] = instanceOf(new OrdClass[Double] {
    def comp(a: Double, b: Double): Ordering =
      if (Eq[Double].equal(a, b))
        EQ
      else if (doubleToRawLongBits(a) < doubleToRawLongBits(b))
        LT
      else
        GT
  })
  implicit val intOrd: Ord[Int] = instanceOf(new OrdClass[Int] {
    def comp(a: Int, b: Int) = java.lang.Integer.compare(a, b) match {
      case 0          => EQ
      case x if x < 0 => LT
      case _          => GT
    }
  })
  implicit val longOrd: Ord[Long] = instanceOf(new OrdClass[Long] {
    def comp(a: Long, b: Long) = java.lang.Long.compare(a, b) match {
      case 0          => EQ
      case x if x < 0 => LT
      case _          => GT
    }
  })
}

trait OrdSyntax {
  implicit final class ToOrdOps[A](a: A) {
    def comp(f: A)(implicit ev: Ord[A]): Ordering = macro meta.Ops.ia_1
  }
}
