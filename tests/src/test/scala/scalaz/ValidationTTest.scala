package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._
import scalaz.ValidationT._

class ValidationTTest extends Spec {
  type ValidationTList[A] = ValidationT[List, String, A]
  type ValidationTT[A, B] = ValidationT[List, A, B]

  checkAll(equal.laws[ValidationTList[Int]])
  checkAll(monad.laws[ValidationTList])
  checkAll(traverse.laws[ValidationTList])
  checkAll(bifunctor.laws[ValidationTT])

  object instances {
    def functor[F[_] : Functor, E] = Functor[({type λ[α] = ValidationT[F, E, α]})#λ]
    def pointed[F[_] : Pointed, E] = Pointed[({type λ[α] = ValidationT[F, E, α]})#λ]
    def apply[F[_] : Apply, E: Semigroup] = Apply[({type λ[α] = ValidationT[F, E, α]})#λ]
    def applicative[F[_] : Applicative, E: Semigroup] = Applicative[({type λ[α] = ValidationT[F, E, α]})#λ]
    def monad[F[_] : Monad, E: Semigroup] = Monad[({type λ[α] = ValidationT[F, E, α]})#λ]
    def monad[F[_] : Monad, E] = Monad[({type λ[α] = ValidationT[F, E, α]})#λ]
    def foldable[F[_] : Foldable, E] = Foldable[({type λ[α] = ValidationT[F, E, α]})#λ]
    def traverse[F[_] : Traverse, E] = Traverse[({type λ[α] = ValidationT[F, E, α]})#λ]

    // check for absence of ambiguity
    def functor[F[_] : Monad, E] = Functor[({type λ[α] = ValidationT[F, E, α]})#λ]
    def pointed[F[_] : Monad, E] = Pointed[({type λ[α] = ValidationT[F, E, α]})#λ]
    def apply[F[_] : Monad, E: Semigroup] = Apply[({type λ[α] = ValidationT[F, E, α]})#λ]
    def applicative[F[_] : Monad, E: Semigroup] = Applicative[({type λ[α] = ValidationT[F, E, α]})#λ]
    def foldable[F[_] : Traverse, E] = Foldable[({type λ[α] = ValidationT[F, E, α]})#λ]

  }
}
