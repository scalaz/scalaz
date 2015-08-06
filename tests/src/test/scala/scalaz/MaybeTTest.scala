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
    implicit def maybeTArb0[F[_, _], E, A](implicit F: Arbitrary[F[E, Maybe[A]]]): Arbitrary[MaybeT[({type l[a] = F[E, a]})#l, A]] =
      maybeTArb[({type l[a] = F[E, a]})#l, A]

    implicit def maybeTEqual0[F[_, _], E, A](implicit F: Equal[F[E, Maybe[A]]]): Equal[MaybeT[({type l[a] = F[E, a]})#l, A]] =
      MaybeT.maybeTEqual[({type l[a] = F[E, a]})#l, A]

    checkAll(monadError.laws[({type x[E0, A] = MaybeT[({type y[a] = E0 \/ a})#y, A]})#x, Int])
  }

  object instances {
    def functor[F[_] : Functor] = Functor[({type λ[α] = MaybeT[F, α]})#λ]
    def monad[F[_] : Monad] = MonadPlus[({type λ[α] = MaybeT[F, α]})#λ]
    def monadError[F[_, _], E](implicit F: MonadError[F, E]) = MonadError[({type x[E0, A] = MaybeT[({type y[a] = F[E0, a]})#y, A]})#x, E]
    def foldable[F[_] : Foldable] = Foldable[({type λ[α] = MaybeT[F, α]})#λ]
    def traverse[F[_] : Traverse] = Traverse[({type λ[α] = MaybeT[F, α]})#λ]

    // checking absence of ambiguity
    def functor[F[_] : Monad] = Functor[({type λ[α] = MaybeT[F, α]})#λ]
    def functor[F[_] : Monad : Traverse] = Functor[({type λ[α] = MaybeT[F, α]})#λ]
    def functor[F[_, _], E](implicit F1: MonadError[F, E], F2: Traverse[({type l[a] = F[E, a]})#l]) = Functor[({type x[a] = MaybeT[({type y[b] = F[E, b]})#y, a]})#x]
    def apply[F[_] : Monad] = Apply[({type λ[α] = MaybeT[F, α]})#λ]
    def foldable[F[_] : Traverse] = Foldable[({type λ[α] = MaybeT[F, α]})#λ]
  }
}
