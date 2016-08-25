package scalaz
package data

import Prelude._
import typeclass.{MonadClass, TraversableClass}
import typeclass.FoldableClass._

// TODO Rework lazyness
trait MaybeInstances extends MonadClass.Template[Maybe] with TraversableClass[Maybe] with FoldRight[Maybe] {

  implicit def monadInstance: Monad[Maybe] = this

  override def ap[A, B](ma: Maybe[A])(mf: Maybe[A => B]): Maybe[B] =
    ma.fold(a => map[A => B, B](mf)(f => f(a)), empty)

  override def flatMap[A, B](ma: Maybe[A])(f: A => Maybe[B]): Maybe[B] =
    ma.fold(a => f(a), empty)

  override def map[A, B](ma: Maybe[A])(f: A => B): Maybe[B] =
    ma.fold(a => just(f(a)), empty)

  override def pure[A](a: A): Maybe[A] =
    just(a)

  override def traverse[F[_], A, B](ma: Maybe[A])(f: A => F[B])(implicit F: Applicative[F]): F[Maybe[B]] =
    ma.fold(a => f(a).map(just(_)), empty.pure[F])

  override def sequence[F[_], A](ma: Maybe[F[A]])(implicit F: Applicative[F]): F[Maybe[A]] =
    ma.fold(fa => fa.map(just(_)), empty.pure[F])

  override def foldLeft[A, B](ma: Maybe[A], b: B)(f: (B, A) => B): B = ma.fold(a => f(b, a), b)

  override def foldRight[A, B](ma: Maybe[A], b: => B)(f: (A, => B) => B): B = ma.fold(a => f(a, b), b)

  override def toList[A](ma: Maybe[A]): List[A] = ma.fold(List(_), Nil)
}
