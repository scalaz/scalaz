package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._

class EitherTTest extends Spec {

  type EitherTList[A, B] = EitherT[List, A, B]
  type EitherTListInt[A] = EitherT[List, Int, A]
  type EitherTOptionInt[A] = EitherT[Option, Int, A]

  checkAll(equal.laws[EitherTListInt[Int]])
  checkAll(monad.laws[EitherTListInt])
  checkAll(traverse.laws[EitherTListInt])
  checkAll(bitraverse.laws[EitherTList])

  object instances {
    def functor[F[+_] : Functor, A] = Functor[({type λ[α] = EitherT[F, A, α]})#λ]
    def monad[F[+_] : Monad, A] = Monad[({type λ[α] = EitherT[F, A, α]})#λ]
    def foldable[F[+_] : Foldable, A] = Foldable[({type λ[α] = EitherT[F, A, α]})#λ]
    def traverse[F[+_] : Traverse, A] = Traverse[({type λ[α] = EitherT[F, A, α]})#λ]

    // checking absence of ambiguity
    def functor[F[+_] : Monad, A] = Functor[({type λ[α] = EitherT[F, A, α]})#λ]
    def apply[F[+_] : Monad, A] = Apply[({type λ[α] = EitherT[F, A, α]})#λ]
    def foldable[F[+_] : Traverse, A] = Foldable[({type λ[α] = EitherT[F, A, α]})#λ]
  }

}
