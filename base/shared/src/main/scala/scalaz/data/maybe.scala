package scalaz
package data

import scala.{ List, None, Option, Some }

import scalaz.core.EqClass
import scalaz.ct._
import scalaz.debug.DebugClass

sealed trait MaybeModule {
  type Maybe[A]

  object Just {
    def unapply[A](ma: Maybe[A]): Option[A] = toOption(ma)
  }

  object Empty {
    def unapply[A](ma: Maybe[A]): Boolean = toOption(ma).isEmpty
  }

  def fold[A, B](ma: Maybe[A])(f: A => B, b: => B): B =
    toOption(ma).fold(b)(f)

  def empty[A]: Maybe[A]
  def just[A](a: A): Maybe[A]
  def maybe[A, B](n: B)(f: A => B): Maybe[A] => B
  def fromOption[A](oa: Option[A]): Maybe[A]
  def toOption[A](ma: Maybe[A]): Option[A]

  /* typeclass instances */
  def isCovariant: IsCovariant[Maybe]
  def monad: Monad[Maybe]
  def traversable: Traversable[Maybe]
  def debug[A: Debug]: Debug[Maybe[A]]
  def eq[A: Eq]: Eq[Maybe[A]]
}

object MaybeModule extends MaybeSyntax {
  implicit def monadMaybe: Monad[Maybe]              = Maybe.monad
  implicit def traversableMaybe: Traversable[Maybe]  = Maybe.traversable
  implicit def isCovariantMaybe: IsCovariant[Maybe]  = Maybe.isCovariant
  implicit def debugMaybe[A: Debug]: Debug[Maybe[A]] = Maybe.debug[A]
}

private[scalaz] object MaybeImpl extends MaybeModule {
  type Maybe[A] = Option[A]

  def empty[A]: Maybe[A]      = None
  def just[A](a: A): Maybe[A] = Some(a)
  def maybe[A, B](n: B)(f: A => B): Maybe[A] => B = {
    case Some(a) => f(a)
    case None    => n
  }
  def fromOption[A](oa: Option[A]): Maybe[A] = oa
  def toOption[A](ma: Maybe[A]): Option[A]   = ma

  def isCovariant: IsCovariant[Maybe] = Scalaz.scalaCovariant[Option]
  def monad: Monad[Maybe]             = instanceOf(instance)
  def traversable: Traversable[Maybe] = instanceOf(instance)

  def debug[A](implicit A: Debug[A]): Debug[Maybe[A]] = instanceOf[DebugClass[Maybe[A]]] {
    case Some(a) => s"Just(${A.debug(a)})"
    case _       => "Empty"
  }

  def eq[A](implicit A: Eq[A]): Eq[Maybe[A]] = instanceOf[EqClass[Maybe[A]]] {
    case (Some(a), Some(b)) => A.equal(a, b)
    case (None, None)       => true
    case _                  => false
  }

  private val instance =
    new MonadClass[Maybe] with BindClass.DeriveFlatten[Maybe] with TraversableClass[Maybe]
    with FoldableClass.DeriveFoldMap[Maybe] {

      override def ap[A, B](ma: Maybe[A])(mf: Maybe[A => B]): Maybe[B] =
        mf match {
          case Some(f) => ma.map(f)
          case _       => None
        }

      override def flatMap[A, B](ma: Maybe[A])(f: A => Maybe[B]): Maybe[B] =
        ma.flatMap(f)

      override def map[A, B](ma: Maybe[A])(f: A => B): Maybe[B] =
        ma.map(f)

      override def pure[A](a: A): Maybe[A] =
        just(a)

      override def traverse[F[_], A, B](ma: Maybe[A])(f: A => F[B])(implicit F: Applicative[F]): F[Maybe[B]] =
        ma match {
          case Some(a) => F.map(f(a))(just)
          case _       => F.pure(None)
        }

      override def sequence[F[_], A](ma: Maybe[F[A]])(implicit F: Applicative[F]): F[Maybe[A]] =
        ma match {
          case Some(fa) => F.map(fa)(just)
          case _        => F.pure(None)
        }

      override def foldLeft[A, B](ma: Maybe[A], b: B)(f: (B, A) => B): B =
        ma match {
          case Some(a) => f(b, a)
          case _       => b
        }

      override def foldRight[A, B](ma: Maybe[A], b: => B)(f: (A, => B) => B): B =
        ma match {
          case Some(a) => f(a, b)
          case _       => b
        }

      override def toList[A](ma: Maybe[A]): List[A] =
        ma.toList
    }
}

trait MaybeFunctions {
  def empty[A]      = Maybe.empty[A]
  def just[A](a: A) = Maybe.just(a)
}

trait MaybeSyntax {
  implicit final class OptionAsMaybe[A](oa: Option[A]) { def asMaybe: Maybe[A] = Maybe.fromOption(oa) }

  implicit final class ToMaybeOps[A](a: A) {
    def just: Maybe[A] = Maybe.just(a)
  }
}
