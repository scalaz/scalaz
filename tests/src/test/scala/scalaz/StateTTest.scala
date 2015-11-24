package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary
import std.AllInstances._

object StateTTest extends SpecLite {

  type StateTList[S, A] = StateT[List, S, A]
  type StateTListInt[A] = StateTList[Int, A]

  implicit def stateTListEqual = Equal[List[(Int, Int)]].contramap((_: StateTListInt[Int]).runZero[Int])
  implicit def stateTListArb = ScalazArbitrary.stateTArb[List, Int, Int]
  implicit def stateTListArb2 = ScalazArbitrary.stateTArb[List, Int, Int => Int]

  checkAll(equal.laws[StateTListInt[Int]])
  checkAll(bindRec.laws[StateTListInt])
  checkAll(monad.laws[StateTListInt])

  object instances {
    def functor[S, F[_] : Functor] = Functor[StateT[F, S, ?]]
    def plus[F[_]: Monad: Plus, S1, S2] = Plus[IndexedStateT[F, S1, S2, ?]]
    def bindRec[S, F[_] : Monad : BindRec] = BindRec[StateT[F, S, ?]]
    def monadState[S, F[_] : Monad] = MonadState[StateT[F, S, ?], S]
    def monadPlus[S, F[_]: MonadPlus] = MonadPlus[StateT[F, S, ?]]

    // F = Id
    def functor[S] = Functor[State[S, ?]]
    def monadState[S] = MonadState[State[S, ?], S]

    // checking absence of ambiguity
    def functor[S, F[_] : Monad] = Functor[StateT[F, S, ?]]
    def plus[F[_]: MonadPlus, S] = Plus[StateT[F, S, ?]]
  }

  "monadState.state" in {
    instances.monadState[Boolean].state(42).run(true) must_===((true, 42))
  }

  "monadState.constantState" in {
    instances.monadState[Boolean].constantState(42, false).run(true) must_===((false, 42))
  }

  "monadState.get" in {
    instances.monadState[Boolean].get.run(true) must_===((true, true))
  }

  "monadState.gets" in {
    instances.monadState[Int].gets { _ + 1 }.run(10) must_===((10, 11))
  }

  "monadState.put" in {
    instances.monadState[Int].put(20).run(10) must_===((20, ()))
  }

  "monadState.modify" in {
    instances.monadState[Int].modify { _ + 1 }.run(10) must_===((11, ()))
  }

  "monadPlus.empty (List)" in {
    instances.monadPlus[Boolean, List].empty[Int].run(false) must_===(Nil)
  }

  "monadPlus.plus (List)" in {
    val a = StateT[List, Int, Boolean](s => List((s, false)))
    val b = StateT[List, Int, Boolean](s => List((s, true)))
    instances.monadPlus[Int, List].plus(a, b).run(0) must_===(List((0, false), (0, true)))
  }

  "StateT can be trampolined without stack overflow" in {
    import scalaz.Free._
    val result = (0 to 4000).toList.map(i => StateT[Trampoline, Int, Int]((ii:Int) => Trampoline.done((i,i))))
      .foldLeft(StateT((s:Int) => Trampoline.done((s,s))))( (a,b) => a.flatMap(_ => b))
    4000 must_=== result(0).run._1
  }
}
