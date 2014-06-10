package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._
import org.scalacheck.Prop.forAll

object MaybeTTest extends SpecLite {

  type MaybeTList[A] = MaybeT[List, A]

  checkAll(equal.laws[MaybeTList[Int]])
  checkAll(monadPlus.laws[MaybeTList])
  checkAll(traverse.laws[MaybeTList])

  object instances {
    def functor[F[_] : Functor] = Functor[({type λ[α] = MaybeT[F, α]})#λ]
    def monad[F[_] : Monad] = MonadPlus[({type λ[α] = MaybeT[F, α]})#λ]
    def foldable[F[_] : Foldable] = Foldable[({type λ[α] = MaybeT[F, α]})#λ]
    def traverse[F[_] : Traverse] = Traverse[({type λ[α] = MaybeT[F, α]})#λ]

    // checking absence of ambiguity
    def functor[F[_] : Monad] = Functor[({type λ[α] = MaybeT[F, α]})#λ]
    def functor[F[_] : Monad : Traverse] = Functor[({type λ[α] = MaybeT[F, α]})#λ]
    def apply[F[_] : Monad] = Apply[({type λ[α] = MaybeT[F, α]})#λ]
    def foldable[F[_] : Traverse] = Foldable[({type λ[α] = MaybeT[F, α]})#λ]
  }
}
