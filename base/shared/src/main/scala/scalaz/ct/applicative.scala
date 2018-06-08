package scalaz
package ct

import scala.language.experimental.macros

trait ApplicativeClass[F[_]] extends ApplyClass[F] {
  def pure[A](a: A): F[A]
}

object ApplicativeClass {

  trait DeriveMap[F[_]] extends ApplicativeClass[F] with MonadClass.Alt[DeriveMap[F]] {
    final override def map[A, B](ma: F[A])(f: (A) => B): F[B] = ap(ma)(pure(f))
  }
}

trait ApplicativeSyntax {
  implicit final class ToApplicativeOps[A](a: A) {
    def pure[F[_]](implicit ev: Applicative[F]): F[A] = macro meta.Ops.i_0
  }
}
