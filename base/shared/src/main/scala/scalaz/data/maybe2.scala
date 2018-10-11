package scalaz
package data

import scala.{ sys, AnyVal, Nothing }
import Predef._
import prop._
import tc._

sealed trait Maybe2Module {

  /**
   * Isomorphic to `Maybe[(A, B)]`, but avoids allocating a `Tuple2` instance.
   */
  type Maybe2[A, B]

  def just2[A, B](a: A, b: B): Maybe2[A, B]
  def empty2[A, B]: Maybe2[A, B]

  object Just2 {
    def unapply[A, B](m: Maybe2[A, B]): Just2Extractor[A, B] = new Just2Extractor(toOption2(m))
  }
  object Empty2 {
    def unapply[A, B](m: Maybe2[A, B]): Boolean = toOption2(m).isEmpty
  }

  def need[A, B](m: () => Maybe2[A, B]): Maybe2[A, B]
  def name[A, B](m: () => Maybe2[A, B]): Maybe2[A, B]

  private[data] def isCovariant_1[B]: IsCovariant[Maybe2[?, B]]
  private[data] def isCovariant_2[A]: IsCovariant[Maybe2[A, ?]]
  private[data] def debug[A: Debug, B: Debug]: Debug[Maybe2[A, B]]
  private[data] def bifunctor: Bifunctor[Maybe2]
  private[data] def eq[A: Eq, B: Eq]: Eq[Maybe2[A, B]]

  private[data] def fromOption2[A, B](o: Option2[A, B]): Maybe2[A, B]
  private[data] def toOption2[A, B](m: Maybe2[A, B]): Option2[A, B]
}

final class Just2Extractor[A, B] private[data] (private val value: Option2[A, B]) extends AnyVal {
  def isEmpty: Boolean          = value.isEmpty
  def get: Just2Extractor[A, B] = this
  def _1: A                     = value._1
  def _2: B                     = value._2
}

object Maybe2Module {
  implicit def isCovariant_1[B]: IsCovariant[Maybe2[?, B]] = Maybe2.isCovariant_1
  implicit def isCovariant_2[A]: IsCovariant[Maybe2[A, ?]] = Maybe2.isCovariant_2
  implicit def eq[A, B](implicit A: Eq[A], B: Eq[B]): Eq[Maybe2[A, B]] =
    Maybe2.eq[A, B]
  implicit def bifunctor: Bifunctor[Maybe2]     = Maybe2.bifunctor
  implicit def delay[A, B]: Delay[Maybe2[A, B]] = instanceOf[DelayClass[Maybe2[A, B]]](Maybe2.need _)
}

private[data] object Maybe2Impl extends Maybe2Module {
  type Maybe2[A, B] = Eval[Option2[A, B]]

  def just2[A, B](a: A, b: B): Maybe2[A, B] = fromOption2(Some2(a, b))
  def empty2[A, B]: Maybe2[A, B]            = Eval.value(None2)

  override def name[A, B](m: () => Maybe2[A, B]): Maybe2[A, B] = Eval.name(m)
  override def need[A, B](m: () => Maybe2[A, B]): Maybe2[A, B] = Eval.need(m)

  def isCovariant_1[B]: IsCovariant[Maybe2[?, B]] = new IsCovariant.LiftLiskov[Maybe2[?, B]] {
    override def liftLiskov[A, C](implicit ev: A <~< C): Maybe2[A, B] <~< Maybe2[C, B] = {
      import Scalaz.ToAsOps
      ev.liftCvF[Option2[?, B]].liftCvF[Eval]
    }
  }
  def isCovariant_2[A]: IsCovariant[Maybe2[A, ?]] = new IsCovariant.LiftLiskov[Maybe2[A, ?]] {
    override def liftLiskov[B, C](implicit ev: B <~< C): Maybe2[A, B] <~< Maybe2[A, C] = {
      import Scalaz.ToAsOps
      ev.liftCvF[Option2[A, ?]].liftCvF[Eval]
    }
  }

  def debug[A, B](implicit A: Debug[A], B: Debug[B]): Debug[Maybe2[A, B]] = {
    import Scalaz.debugInterpolator
    DebugClass.instance[Maybe2[A, B]] {
      case Eval(Some2(a, b)) => z"Just2($a, $b)"
      case _                 => Cord("Empty2")
    }
  }

  def eq[A, B](implicit A: Eq[A], B: Eq[B]): Eq[Maybe2[A, B]] =
    instanceOf[EqClass[Maybe2[A, B]]] {
      case (Eval(Some2(a1, b1)), Eval(Some2(a2, b2))) => A.equal(a1, a2) && B.equal(b1, b2)
      case (Eval(None2), Eval(None2))                 => true
      case _                                          => false
    }

  def bifunctor: Bifunctor[Maybe2] =
    instanceOf(new BifunctorClass[Maybe2] {
      override def bimap[A, B, S, T](fab: Maybe2[A, B])(as: A => S, bt: B => T): Maybe2[S, T] =
        fab match {
          case Eval(Some2(_1, _2)) => Eval.value(Some2(as(_1), bt(_2)))
          case _                   => Eval.value(None2)
        }
    })

  private[data] def fromOption2[A, B](o: Option2[A, B]): Maybe2[A, B] = Eval.value(o)
  private[data] def toOption2[A, B](m: Maybe2[A, B]): Option2[A, B]   = Eval.eval(m)
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

private[data] object Option2 {
  implicit def isCovariant_1[B]: IsCovariant[Option2[?, B]] = IsCovariant.scalaCovariant[Option2[+?, B]]
  implicit def isCovariant_2[A]: IsCovariant[Option2[A, ?]] = IsCovariant.scalaCovariant[Option2[A, +?]]
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
