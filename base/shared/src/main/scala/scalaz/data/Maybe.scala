package scalaz
package data

import typeclass.{MonadClass, TraversableClass}
import typeclass.FoldableClass._

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
  def show[A: Show]: Show[Maybe[A]]
}

object MaybeModule extends MaybeSyntax {
  implicit def monadMaybe: Monad[Maybe] = Maybe.monad
  implicit def traversableMaybe: Traversable[Maybe] = Maybe.traversable
  implicit def isCovariantMaybe: IsCovariant[Maybe] = Maybe.isCovariant
  implicit def showMaybe[A: Show]: Show[Maybe[A]] = Maybe.show[A]
}

private[scalaz] object MaybeImpl extends MaybeModule {
  type Maybe[A] = Option[A]

  def empty[A]: Maybe[A] = None
  def just[A](a: A): Maybe[A] = Some(a)
  def maybe[A, B](n: B)(f: A => B): Maybe[A] => B = _ match {
    case Some(a) => f(a)
    case None    => n
  }
  def fromOption[A](oa: Option[A]): Maybe[A] = oa
  def toOption[A](ma: Maybe[A]): Option[A] = ma

  def isCovariant: IsCovariant[Maybe] = Scalaz.scalaCovariant[Option]
  def monad: Monad[Maybe] = instance
  def traversable: Traversable[Maybe] = instance

  def show[A](implicit A: Show[A]): Show[Maybe[A]] = {
    case Some(a) => s"Just(${A.show(a)})"
    case None    =>  "Empty"
  }

  private val instance =
    new MonadClass.Template[Maybe] with TraversableClass[Maybe] with FoldRight[Maybe] {

      override def ap[A, B](ma: Maybe[A])(mf: Maybe[A => B]): Maybe[B] =
        mf match {
          case Some(f) => ma.map(f)
          case None    => None
        }

      override def flatMap[A, B](ma: Maybe[A])(f: A => Maybe[B]): Maybe[B] =
        ma.flatMap(f)

      override def map[A, B](ma: Maybe[A])(f: A => B): Maybe[B] =
        ma.map(f)

      override def pure[A](a: A): Maybe[A] =
        just(a)

      override def traverse[F[_], A, B](ma: Maybe[A])(f: A => F[B])(implicit F: Applicative[F]): F[Maybe[B]] =
        ma match {
          case Some(a) => F.apply.functor.map(f(a))(just)
          case None    => F.pure(None)
        }

      override def sequence[F[_], A](ma: Maybe[F[A]])(implicit F: Applicative[F]): F[Maybe[A]] =
        ma match {
          case Some(fa) => F.apply.functor.map(fa)(just)
          case None     => F.pure(None)
        }

      override def foldLeft[A, B](ma: Maybe[A], b: B)(f: (B, A) => B): B =
        ma match {
          case Some(a) => f(b, a)
          case None    => b
        }

      override def foldRight[A, B](ma: Maybe[A], b: => B)(f: (A, => B) => B): B =
        ma match {
          case Some(a) => f(a, b)
          case None    => b
        }

      override def toList[A](ma: Maybe[A]): List[A] =
        ma.toList
    }
}
