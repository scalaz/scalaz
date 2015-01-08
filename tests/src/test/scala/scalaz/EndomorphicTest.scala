package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.Arbitrary
import KleisliTest._
import CokleisliTest._
import std.AllInstances._

object EndomorphicTest extends SpecLite {

  implicit def endoEqual[F[_], G[_[_], _, _], A](
    implicit F: Equal[G[F, A, A]]
  ): Equal[Endomorphic[G[F, ?, ?], A]] =
    Equal.equalBy(_.run)

  implicit def endoArb[F[_], G[_[_], _, _], A](
    implicit F: Arbitrary[G[F, A, A]]
  ): Arbitrary[Endomorphic[G[F, ?, ?], A]] =
    Functor[Arbitrary].map(F)(Endomorphic[G[F, ?, ?], A](_))

  checkAll(monoid.laws[Endomorphic[Kleisli[Option, ?, ?], Int]])
  checkAll(semigroup.laws[Endomorphic[Cokleisli[Option, ?, ?], Int]])

  object instances{
    def semigroup[F[_, _]: Compose, A] = Semigroup[Endomorphic[F, A]]
    def monoid[F[_, _]: Category, A] = Monoid[Endomorphic[F, A]]

    def semigroup[F[_, _]: Category, A] = Semigroup[Endomorphic[F, A]]

    object kleisli {
      def semigroup[F[_]: Bind, A] = Semigroup[Endomorphic[Kleisli[F, ?, ?], A]]
      def monoid[F[_]: Monad, A] = Monoid[Endomorphic[Kleisli[F, ?, ?], A]]

      def semigroup[F[_]: Monad, A] = Semigroup[Endomorphic[Kleisli[F, ?, ?], A]]
    }

    object cokleisli {
      def semigroup[F[_]: Cobind, A] = Semigroup[Endomorphic[Cokleisli[F, ?, ?], A]]
      def monoid[F[_]: Comonad, A] = Monoid[Endomorphic[Cokleisli[F, ?, ?], A]]

      def semigroup[F[_]: Comonad, A] = Semigroup[Endomorphic[Cokleisli[F, ?, ?], A]]
    }
  }
}
