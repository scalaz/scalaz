package scalaz
package data

import scala.{ inline, Either }

import Predef._
import tc._

final case class -\/[L, R](value: L) extends (L Disjunction R)
final case class \/-[L, R](value: R) extends (L Disjunction R)

sealed trait Disjunction[L, R] {
  final def fold[A](la: L => A, ra: R => A): A = this match {
    case -\/(l) => la(l)
    case \/-(r) => ra(r)
  }
}

object Disjunction extends DisjunctionFunctions {
  object Syntax extends DisjunctionSyntax
  def swap[L, R](ab: L \/ R): R \/ L = ab.fold(\/-(_), -\/(_))

  def fromEither[L, R](ab: Either[L, R]): L \/ R = ab.fold(-\/(_), \/-(_))

  implicit def disjunctionMonad[L]: Monad[L \/ ?] =
    instanceOf(new MonadClass[L \/ ?] with BindClass.DeriveFlatten[L \/ ?] {

      override def map[A, B](ma: L \/ A)(f: A => B): L \/ B =
        ma.fold(l => -\/(l), r => \/-(f(r)))

      override def ap[A, B](ma: L \/ A)(mf: L \/ (A => B)): L \/ B =
        ma.fold(l => -\/(l), a => map[(A => B), B](mf)(f => f(a)))

      override def pure[A](a: A): L \/ A =
        \/-[L, A](a)

      override def flatMap[A, B](oa: L \/ A)(f: A => L \/ B): L \/ B =
        oa.fold(l => -\/(l), a => f(a))
    })

  implicit def disjunctionDebug[L, R](implicit L: Debug[L], R: Debug[R]): Debug[L \/ R] = {
    import Scalaz.debugInterpolator
    DebugClass.instance[L \/ R] {
      case -\/(l) => z"-\\/($l)"
      case \/-(r) => z"\\/-($r)"
    }
  }

  implicit val disjunctionBifunctor: Bifunctor[\/] =
    instanceOf(new BifunctorClass[\/] with BifunctorClass.DeriveLmapRmap[\/] {
      def bimap[A, B, S, T](fab: A \/ B)(as: A => S, bt: B => T): S \/ T = fab match {
        case -\/(a) => -\/(as(a))
        case \/-(b) => \/-(bt(b))
      }
    })

  implicit def disjunctionEq[A, B](implicit A: Eq[A], B: Eq[B]): Eq[A \/ B] =
    instanceOf[EqClass[A \/ B]] {
      case (-\/(a1), -\/(a2)) => A.equal(a1, a2)
      case (\/-(b1), \/-(b2)) => B.equal(b1, b2)
      case _                  => false
    }
}

trait DisjunctionFunctions {
  @inline def left[L, R](value: L): \/[L, R]  = -\/(value)
  @inline def right[L, R](value: R): \/[L, R] = \/-(value)

  def either[A, B, C](ac: A => C)(bc: B => C): A \/ B => C = _ match {
    case -\/(l) => ac(l)
    case \/-(r) => bc(r)
  }
}

trait DisjunctionSyntax {
  implicit final class ToDisjunctionOps[A](a: A) {
    def left[B]: A \/ B  = -\/(a)
    def right[B]: B \/ A = \/-(a)
  }

  implicit final class EitherAsDisjunction[A, B](ab: Either[A, B]) {
    def asDisjunction: A \/ B = \/.fromEither(ab)
  }
}
