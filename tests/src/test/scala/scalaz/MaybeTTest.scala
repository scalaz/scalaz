package scalaz

import org.scalacheck.Arbitrary
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._

object MaybeTTest extends SpecLite {

  type MaybeTList[A] = MaybeT[List, A]

  checkAll(equal.laws[MaybeTList[Int]])
  checkAll(monadPlus.laws[MaybeTList])
  checkAll(traverse.laws[MaybeTList])

  {
    implicit def maybeTArb0[F[_, _], E, A](implicit F: Arbitrary[F[E, Maybe[A]]]): Arbitrary[MaybeT[F[E, ?], A]] =
      maybeTArb[F[E, ?], A]

    implicit def maybeTEqual0[F[_, _], E, A](implicit F: Equal[F[E, Maybe[A]]]): Equal[MaybeT[F[E, ?], A]] =
      MaybeT.maybeTEqual[F[E, ?], A]

    checkAll(monadError.laws[λ[(E0, A) => MaybeT[E0 \/ ?, A]], Int])
  }

  object instances {
    def functor[F[_] : Functor] = Functor[MaybeT[F, ?]]
    def monad[F[_] : Monad] = MonadPlus[MaybeT[F, ?]]
    def monadError[F[_, _], E](implicit F: MonadError[F, E]) = MonadError[λ[(E0, A) => MaybeT[F[E0, ?], A]], E]
    def foldable[F[_] : Foldable] = Foldable[MaybeT[F, ?]]
    def traverse[F[_] : Traverse] = Traverse[MaybeT[F, ?]]

    // checking absence of ambiguity
    def functor[F[_] : Monad] = Functor[MaybeT[F, ?]]
    def functor[F[_] : Monad : Traverse] = Functor[MaybeT[F, ?]]
    def functor[F[_, _], E](implicit F1: MonadError[F, E], F2: Traverse[F[E, ?]]) = Functor[MaybeT[λ[A => F[E, A]], ?]]
    def apply[F[_] : Monad] = Apply[MaybeT[F, ?]]
    def foldable[F[_] : Traverse] = Foldable[MaybeT[F, ?]]
  }
}
