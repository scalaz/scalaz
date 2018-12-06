package scalaz
package tc

import scala.annotation.tailrec
import scala.language.experimental.macros

import data.Maybe
import data.Maybe.Just

/**
 * Universally quantified [[scalaz.Semigroup]].
 */
trait PlusClass[F[_]] { self =>

  def plus[A](a: F[A], b: => F[A]): F[A]

  /**
   * Unfold `seed` to the left and sum using [[#plus]].
   * `Plus` instances with right absorbing elements may override this method
   * to not unfold more than is necessary to determine the result.
   */
  def unfoldlPsumOpt[S, A](seed: S)(f: S => Maybe[(S, F[A])]): Maybe[F[A]] = {
    @tailrec def go(s: S, acc: F[A]): F[A] =
      f(s) match {
        case Just((s, fa)) => go(s, plus(fa, acc))
        case _             => acc
      }
    Maybe.monad.map(f(seed)) { case (s, a) => go(s, a) }
  }

  /**
   * Unfold `seed` to the right and sum using [[#plus]].
   * `Plus` instances with left absorbing elements may override this method
   * to not unfold more than is necessary to determine the result.
   */
  def unfoldrPsumOpt[S, A](seed: S)(f: S => Maybe[(F[A], S)]): Maybe[F[A]] = {
    @tailrec def go(acc: F[A], s: S): F[A] =
      f(s) match {
        case Just((fa, s)) => go(plus(acc, fa), s)
        case _             => acc
      }
    Maybe.monad.map(f(seed)) { case (a, s) => go(a, s) }
  }

  def semigroup[A]: Semigroup[F[A]] = instanceOf {
    new SemigroupClass[F[A]] {
      def mappend(f1: F[A], f2: => F[A]): F[A] = plus(f1, f2)
    }
  }
}

trait PlusFunctions {
  def plus[F[_], A](f1: F[A], f2: => F[A])(implicit F: Plus[F]): F[A] =
    F.plus(f1, f2)

  def unfoldlPsumOpt[F[_], S, A](seed: S)(f: S => Maybe[(S, F[A])])(implicit F: Plus[F]): Maybe[F[A]] =
    F.unfoldlPsumOpt(seed)(f)

  def unfoldrPsumOpt[F[_], S, A](seed: S)(f: S => Maybe[(F[A], S)])(implicit F: Plus[F]): Maybe[F[A]] =
    F.unfoldrPsumOpt(seed)(f)

  def semigroup[F[_], A](implicit F: Plus[F]): Semigroup[F[A]] =
    F.semigroup[A]
}

trait PlusSyntax {

  implicit final class ToPlusOps[F[_], A](private val self: F[A]) {

    def <+>(f: => F[A])(implicit ev: Plus[F]): F[A] =
      macro ops.Ops.ia_1

    def plus(f: => F[A])(implicit ev: Plus[F]): F[A] =
      macro ops.Ops.ia_1
  }
}
