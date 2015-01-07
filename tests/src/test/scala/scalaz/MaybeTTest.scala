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
    def functor[F[_] : Functor] = Functor[MaybeT[F, ?]]
    def monad[F[_] : Monad] = MonadPlus[MaybeT[F, ?]]
    def foldable[F[_] : Foldable] = Foldable[MaybeT[F, ?]]
    def traverse[F[_] : Traverse] = Traverse[MaybeT[F, ?]]

    // checking absence of ambiguity
    def functor[F[_] : Monad] = Functor[MaybeT[F, ?]]
    def functor[F[_] : Monad : Traverse] = Functor[MaybeT[F, ?]]
    def apply[F[_] : Monad] = Apply[MaybeT[F, ?]]
    def foldable[F[_] : Traverse] = Foldable[MaybeT[F, ?]]
  }
}
