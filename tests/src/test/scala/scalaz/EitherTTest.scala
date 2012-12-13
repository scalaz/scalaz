package scalaz

import scalaz.testlib.ScalazProperties._
import scalaz.testlib.ScalazArbitrary._
import std.AllInstances._

class EitherTTest extends testlib.Spec {

  type EitherTListInt[A] = EitherT[List, Int, A]
  type EitherTOptionInt[A] = EitherT[Option, Int, A]

  checkAll(equal.laws[EitherTListInt[Int]])
  checkAll(monad.laws[EitherTListInt])
  checkAll(traverse.laws[EitherTListInt])

  object instances {
    def functor[F[+_] : Functor, A] = Functor[({type λ[α] = EitherT[F, A, α]})#λ]
    def pointed[F[+_] : Pointed, A] = Pointed[({type λ[α] = EitherT[F, A, α]})#λ]
    def apply[F[+_] : Apply, A] = Apply[({type λ[α] = EitherT[F, A, α]})#λ]
    def applicative[F[+_] : Applicative, A] = Applicative[({type λ[α] = EitherT[F, A, α]})#λ]
    def monad[F[+_] : Monad, A] = Monad[({type λ[α] = EitherT[F, A, α]})#λ]
    def foldable[F[+_] : Foldable, A] = Foldable[({type λ[α] = EitherT[F, A, α]})#λ]
    def traverse[F[+_] : Traverse, A] = Traverse[({type λ[α] = EitherT[F, A, α]})#λ]

    // checking absence of ambiguity
    def functor[F[+_] : Monad, A] = Functor[({type λ[α] = EitherT[F, A, α]})#λ]
    def pointed[F[+_] : Monad, A] = Pointed[({type λ[α] = EitherT[F, A, α]})#λ]
    def apply[F[+_] : Monad, A] = Apply[({type λ[α] = EitherT[F, A, α]})#λ]
    def foldable[F[+_] : Traverse, A] = Foldable[({type λ[α] = EitherT[F, A, α]})#λ]
  }

}
