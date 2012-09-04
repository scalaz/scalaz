package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary
import scalaz.scalacheck.ScalazArbitrary.{stateTArb => _, _}
import std.AllInstances._
import Id._

class StateTTest extends Spec {

  type StateTList[S, A] = StateT[List, S, A]
  type StateTListInt[A] = StateTList[Int, A]

  implicit def stateTListEqual = Equal[List[(Int, Int)]].contramap((_: StateTListInt[Int]).runZero)
  implicit def stateTListArb = ScalazArbitrary.stateTArb[List, Int, Int]
  implicit def stateTListArb2 = ScalazArbitrary.stateTArb[List, Int, Int => Int]

  checkAll(equal.laws[StateTListInt[Int]])
  checkAll(monad.laws[StateTListInt])

  object instances {
    def functor[S, F[+_] : Functor] = Functor[({type λ[+α] = StateT[F, S, α]})#λ]
    def pointed[S, F[+_] : Pointed] = Pointed[({type λ[+α] = StateT[F, S, α]})#λ]
    def monadState[S, F[+_] : Monad] = MonadState[({type λ[α, +β]=StateT[F, α, β]})#λ, S]

    // F = Id
    def functor[S] = Functor[({type λ[α] = State[S, α]})#λ]
    def pointed[S] = Pointed[({type λ[α] = State[S, α]})#λ]
    def monadState[S] = MonadState[({type λ[α, β]=State[α, β]})#λ, S]

    // checking absence of ambiguity
    def functor[S, F[+_] : Monad] = Functor[({type λ[+α] = StateT[F, S, α]})#λ]
    def pointed[S, F[+_] : Monad] = Pointed[({type λ[+α] = StateT[F, S, α]})#λ]
  }
}
