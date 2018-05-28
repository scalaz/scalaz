package scalaz
package data

import scala.{ sys, AnyVal, Nothing }

import scalaz.core.EqClass
import scalaz.ct.BifunctorClass
import scalaz.debug.DebugClass

sealed trait Maybe2Module {

  /**
   * Isomorphic to `Maybe[(A, B)]`, but avoids allocating a `Tuple2` isntance.
   */
  type Maybe2[A, B]

  def just2[A, B](a: A, b: B): Maybe2[A, B]
  def empty2[A, B]: Maybe2[A, B]

  implicit def isCovariant_1[B]: IsCovariant[Maybe2[?, B]]
  implicit def isCovariant_2[A]: IsCovariant[Maybe2[A, ?]]
  implicit def debug[A: Debug, B: Debug]: Debug[Maybe2[A, B]]
  implicit def bifunctor: Bifunctor[Maybe2]

  implicit def eq[A: Eq, B: Eq]: Eq[Maybe2[A, B]]

  object Just2 {
    def unapply[A, B](m: Maybe2[A, B]): Just2Extractor[A, B] = new Just2Extractor(toOption2(m))
  }
  object Empty2 {
    def unapply[A, B](m: Maybe2[A, B]): Boolean = toOption2(m).isEmpty
  }

  private[data] def fromOption2[A, B](o: Option2[A, B]): Maybe2[A, B]
  private[data] def toOption2[A, B](m: Maybe2[A, B]): Option2[A, B]
}

final class Just2Extractor[A, B] private[data] (private val value: Option2[A, B]) extends AnyVal {
  def isEmpty: Boolean          = value.isEmpty
  def get: Just2Extractor[A, B] = this
  def _1: A                     = value._1
  def _2: B                     = value._2
}

private[data] object Maybe2Impl extends Maybe2Module {
  type Maybe2[A, B] = Option2[A, B]

  def just2[A, B](a: A, b: B): Maybe2[A, B] = Maybe2Impl.fromOption2(Some2(a, b))
  def empty2[A, B]: Maybe2[A, B]            = None2

  implicit def isCovariant_1[B]: IsCovariant[Maybe2[?, B]] = Scalaz.scalaCovariant[Option2[+?, B]]
  implicit def isCovariant_2[A]: IsCovariant[Maybe2[A, ?]] = Scalaz.scalaCovariant[Option2[A, +?]]

  implicit def debug[A, B](implicit A: Debug[A], B: Debug[B]): Debug[Maybe2[A, B]] =
    instanceOf[DebugClass[Maybe2[A, B]]] {
      case Some2(_1, _2) => s"Just2(${A.debug(_1)}, ${B.debug(_2)})"
      case _             => "Empty2"
    }

  implicit def eq[A, B](implicit A: Eq[A], B: Eq[B]): Eq[Maybe2[A, B]] =
    instanceOf[EqClass[Maybe2[A, B]]] {
      case (Some2(a1, b1), Some2(a2, b2)) => A.equal(a1, a2) && B.equal(b1, b2)
      case (None2, None2)                 => true
      case _                              => false
    }

  implicit def bifunctor: scalaz.Bifunctor[Maybe2] =
    instanceOf(new BifunctorClass[Maybe2] with BifunctorClass.DeriveLmapRmap[Maybe2] {
      def bimap[A, B, S, T](fab: Maybe2[A, B])(as: A => S, bt: B => T): Maybe2[S, T] =
        fab match {
          case Some2(_1, _2) => Some2(as(_1), bt(_2))
          case None2         => None2
        }
    })

  private[data] def fromOption2[A, B](o: Option2[A, B]): Maybe2[A, B] = o
  private[data] def toOption2[A, B](m: Maybe2[A, B]): Option2[A, B]   = m
}

/**
 * Isomorphic to `Option[(A, B)]`, but avoids allocating a `Tuple2` instance.
 *
 * Covariance is intentional. For invariant version, see [[data.Maybe2]].
 */
private[data] sealed abstract class Option2[+A, +B] {
  def isEmpty: Boolean = this eq None2
  def _1: A
  def _2: B
}

private[data] case class Some2[+A, +B](_1: A, _2: B) extends Option2[A, B]

private[data] case object None2 extends Option2[Nothing, Nothing] {
  def _1: Nothing = sys.error("unreachable code")
  def _2: Nothing = sys.error("unreachable code")
}

trait Maybe2Syntax {

  def empty2[A, B]            = Maybe2.empty2[A, B]
  def just2[A, B](a: A, b: B) = Maybe2.just2(a, b)
}
