package scalaz
package ct

import scala.language.experimental.macros
import scala.AnyVal

import data.~>

trait ApplicativeClass[F[_]] extends ApplyClass[F] {
  def pure[A](a: A): F[A]
}

object ApplicativeClass {

  trait DeriveMap[F[_]] extends ApplicativeClass[F] with MonadClass.Alt[DeriveMap[F]] {
    final override def map[A, B](ma: F[A])(f: (A) => B): F[B] = ap(ma)(pure(f))
  }
}

trait ApplicativeFunctions {
  @inline final def pure[F[_], A](a: A)(implicit F: Applicative[F]): F[A] =
    F.pure(a)
  @inline final def pure0[F[_]]: ApplicativeFunctions.pure0[F] =
    new ApplicativeFunctions.pure0[F]
  @inline final def pureNT[F[_]](implicit F: Applicative[F]): Id ~> F =
    ∀.mk[Id ~> F].apply(F.pure)
}

object ApplicativeFunctions {
  final class pure0[F[_]] private[ct] (private val u: Unit = ()) extends AnyVal {
    @inline def apply[A](a: A)(implicit F: Applicative[F]): F[A] =
      F.pure(a)
  }
}

trait ApplicativeSyntax {
  implicit final class ToApplicativeOps[A](a: A) {
    def pure[F[_]](implicit ev: Applicative[F]): F[A] = macro meta.Ops.i_0
  }
}
