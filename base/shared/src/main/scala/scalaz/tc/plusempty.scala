package scalaz
package tc

import Predef._
import data.Maybe

/**
 * Universally quantified [[scalaz.Monoid]].
 */
trait PlusEmptyClass[F[_]] extends PlusClass[F] { self =>

  def empty[A]: F[A]

  def unfoldlPsum[S, A](seed: S)(f: S => Maybe[(S, F[A])]): F[A] =
    unfoldlPsumOpt(seed)(f).cata(identity)(empty)

  def unfoldrPsum[S, A](seed: S)(f: S => Maybe[(F[A], S)]): F[A] =
    unfoldrPsumOpt(seed)(f).cata(identity)(empty)

  // derived functions

  def monoid[A]: Monoid[F[A]] = instanceOf {
    new MonoidClass[F[A]] {
      def mappend(f1: F[A], f2: => F[A]): F[A] =
        plus(f1, f2)

      def mempty: F[A] =
        empty[A]
    }
  }
}

trait PlusEmptyFunctions {

  def empty[F[_], A](implicit F: PlusEmpty[F]): F[A] =
    F.empty[A]

  def unfoldlPsum[F[_], S, A](seed: S)(f: S => Maybe[(S, F[A])])(implicit F: PlusEmpty[F]): F[A] =
    F.unfoldlPsum(seed)(f)

  def unfoldrPsum[F[_], S, A](seed: S)(f: S => Maybe[(F[A], S)])(implicit F: PlusEmpty[F]): F[A] =
    F.unfoldrPsum(seed)(f)

  def monoid[F[_], A](implicit F: PlusEmpty[F]): Monoid[F[A]] =
    F.monoid[A]
}
