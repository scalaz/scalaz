package scalaz
package tc

import scala.util.Either
import scala.util.Left
import scala.util.Right

trait ApplicativeErrorClass[F[_], S] extends ApplicativeClass[F] {
  def raiseError[A](e: S): F[A]

  def handleError[A](fa: F[A])(f: S => F[A]): F[A]
}

object ApplicativeError {

  implicit def either[E]: ApplicativeError[Either[E, ?], E] = instanceOf {
    new ApplicativeErrorClass[Either[E, ?], E] {

      override def pure[A](a: A): Either[E, A] =
        Right(a)

      override def ap[A, B](fa: Either[E, A])(ff: Either[E, A => B]): Either[E, B] =
        (fa, ff) match {
          case (Right(a), Right(f)) => Right(f(a))
          case (e @ Left(_), _)     => e.asInstanceOf[Either[E, B]]
          case (_, e @ Left(_))     => e.asInstanceOf[Either[E, B]]
        }

      def raiseError[A](e: E): Either[E, A] =
        Left(e)

      def handleError[A](fa: Either[E, A])(f: E => Either[E, A]): Either[E, A] =
        fa match {
          case Right(_) => fa
          case Left(e)  => f(e)
        }
    }
  }
}
