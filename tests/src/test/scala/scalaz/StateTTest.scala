package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary
import std.AllInstances._

class StateTTest extends Spec {

  type StateTList[S, A] = StateT[List, S, A]
  type StateTListInt[A] = StateTList[Int, A]

  implicit def stateTListEqual = Equal[List[(Int, Int)]].contramap((_: StateTListInt[Int]).runZero[Int])
  implicit def stateTListArb = ScalazArbitrary.stateTArb[List, Int, Int]
  implicit def stateTListArb2 = ScalazArbitrary.stateTArb[List, Int, Int => Int]

  checkAll(equal.laws[StateTListInt[Int]])
  checkAll(monad.laws[StateTListInt])

  object instances {
    def functor[S, F[_] : Functor] = Functor[({type λ[α] = StateT[F, S, α]})#λ]
    def monadState[S, F[_] : Monad] = MonadState[({type λ[α, β]=StateT[F, α, β]})#λ, S]

    // F = Id
    def functor[S] = Functor[({type λ[α] = State[S, α]})#λ]
    def monadState[S] = MonadState[({type λ[α, β]=State[α, β]})#λ, S]

    // checking absence of ambiguity
    def functor[S, F[_] : Monad] = Functor[({type λ[α] = StateT[F, S, α]})#λ]
  }

  "monadState.state" in {
    instances.monadState[Boolean].state(42).run(true) must be_===((true, 42))
  }

  "monadState.constantState" in {
    instances.monadState[Boolean].constantState(42, false).run(true) must be_===((false, 42))
  }

  "monadState.get" in {
    instances.monadState[Boolean].get.run(true) must be_===((true, true))
  }

  "monadState.gets" in {
    instances.monadState[Int].gets { _ + 1 }.run(10) must be_===((10, 11))
  }

  "monadState.put" in {
    instances.monadState[Int].put(20).run(10) must be_===((20, ()))
  }

  "monadState.modify" in {
    instances.monadState[Int].modify { _ + 1 }.run(10) must be_===((11, ()))
  }
}
