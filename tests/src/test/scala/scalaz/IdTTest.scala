package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

object IdTTest extends SpecLite {

  type ConstInt[A] = Const[Int, A]

  checkAll(divisible.laws[IdT[ConstInt,*]])

  object instances {
    def equal[F[_], A](implicit F: Equal[F[A]]) = Equal[IdT[F, A]]
    def order[F[_], A](implicit F: Order[F[A]]) = Order[IdT[F, A]]
    def functor[F[_] : Functor] = Functor[IdT[F, *]]
    def apply[F[_] : Apply] = Apply[IdT[F, *]]
    def bindRec[F[_] : BindRec] = BindRec[IdT[F, *]]
    def monad[F[_] : Monad] = Monad[IdT[F, *]]
    def foldable[F[_] : Foldable] = Foldable[IdT[F, *]]
    def traverse[F[_] : Traverse] = Traverse[IdT[F, *]]
    def decidable[F[_] : Decidable] = Decidable[IdT[F,*]]
    def divisible[F[_] : Divisible] = Divisible[IdT[F,*]]

    // checking absence of ambiguity
    def equal[F[_], A](implicit F: Order[F[A]]) = Equal[IdT[F, A]]
    def functor[F[_] : Monad] = Functor[IdT[F, *]]
    def functor[F[_] : Monad : Traverse] = Functor[IdT[F, *]]
    def functor[F[_] : BindRec: Traverse] = Functor[IdT[F, *]]
    def functor[F[_] : Apply: BindRec] = Functor[IdT[F, *]]
    def functor[F[_] : Applicative: BindRec] = Functor[IdT[F, *]]
    def functor[F[_] : Monad : BindRec] = Functor[IdT[F, *]]
    def apply[F[_] : Monad] = Apply[IdT[F, *]]
    def apply[F[_] : BindRec] = Apply[IdT[F, *]]
    def apply[F[_] : Applicative: BindRec] = Apply[IdT[F, *]]
    def apply[F[_] : ApplicativePlus: BindRec] = Apply[IdT[F, *]]
    def apply[F[_] : Monad: BindRec] = Apply[IdT[F, *]]
    def foldable[F[_] : Traverse] = Foldable[IdT[F, *]]
    def divisible[F[_] : Decidable] = Divisible[IdT[F,*]]
  }
}
