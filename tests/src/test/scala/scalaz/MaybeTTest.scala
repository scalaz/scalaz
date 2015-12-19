package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._

object MaybeTTest extends SpecLite {

  type MaybeTList[A] = MaybeT[List, A]
  type IntOr[A] = Int \/ A
  type MaybeTEither[A] = MaybeT[IntOr, A]

  checkAll(equal.laws[MaybeTList[Int]])
  checkAll(bindRec.laws[MaybeTList])
  checkAll(monadPlus.laws[MaybeTList])
  checkAll(traverse.laws[MaybeTList])
  checkAll(monadError.laws[MaybeTEither, Int])

  object instances {
    def functor[F[_] : Functor] = Functor[MaybeT[F, ?]]
    def monad[F[_] : Monad] = MonadPlus[MaybeT[F, ?]]
    def bindRec[F[_] : Monad : BindRec] = BindRec[MaybeT[F, ?]]
    def monadError[F[_], E](implicit F: MonadError[F, E]) = MonadError[MaybeT[F, ?], E]
    def foldable[F[_] : Foldable] = Foldable[MaybeT[F, ?]]
    def traverse[F[_] : Traverse] = Traverse[MaybeT[F, ?]]

    // checking absence of ambiguity
    def functor[F[_] : Monad] = Functor[MaybeT[F, ?]]
    def functor[F[_] : Monad : Traverse] = Functor[MaybeT[F, ?]]
    def functor[F[_], E](implicit F1: MonadError[F, E], F2: Traverse[F]) = Functor[MaybeT[F, ?]]
    def apply[F[_] : Monad] = Apply[MaybeT[F, ?]]
    def foldable[F[_] : Traverse] = Foldable[MaybeT[F, ?]]
    def monadEither[E] = Monad[MaybeT[E \/ ?, ?]]
  }
}
