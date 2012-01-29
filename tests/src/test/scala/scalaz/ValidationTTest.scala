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
    def functor[F[_] : Functor] = Functor[({type λ[α] = ValidationT[F, String, α]})#λ]
    def pointed[F[_] : Pointed] = Pointed[({type λ[α] = ValidationT[F, String, α]})#λ]
    def apply[F[_] : Apply] = Apply[({type λ[α] = ValidationT[F, String, α]})#λ]
    def monad[F[_] : Monad] = Monad[({type λ[α] = ValidationT[F, String, α]})#λ]
    def foldable[F[_] : Foldable] = Foldable[({type λ[α] = ValidationT[F, String, α]})#λ]
    def traverse[F[_] : Traverse] = Traverse[({type λ[α] = ValidationT[F, String, α]})#λ]
  }
}