package scalaz
package data

import typeclass.{MonadClass, TraversableClass}
import typeclass.FoldableClass._

trait MaybeInstances {
  implicit def maybeShow[A](implicit S: Show[A]): Show[Maybe[A]] = new Show[Maybe[A]] {
    def show(a: Maybe[A]) = a match {
      case Some(a) => s"Just(${S.show(a)})"
      case None    =>  "Empty"
    }
  }

  implicit val maybeIsCovariant: IsCovariant[Maybe] = Scalaz.scalaCovariant[Option]

  implicit val maybeTraversable: Traversable[Maybe] = new TraversableClass[Maybe] with FoldRight[Maybe] {
      def just[A](a: A): Maybe[A] = Some(a)

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

      override def map[A, B](ma: Maybe[A])(f: A => B): Maybe[B] =
        ma.map(f)
  }

  implicit val maybeMonad: Monad[Maybe] =
    new MonadClass.Template[Maybe] {
      def just[A](a: A): Maybe[A] = Some(a)

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
    }
}