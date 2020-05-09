package scalaz

import org.scalacheck.Arbitrary
import scalaz.Id._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._

object StateTTest extends SpecLite {

  type StateTList[S, A] = StateT[S, List, A]
  type StateTListInt[A] = StateTList[Int, A]
  type IStateTList[S, A] = IndexedStateT[S, Int, List, A]
  type ConstInt[A] = Const[Int, A]

  private[this] val stateTestInts = (-10 to 10).toList

  private[this] implicit def stateTIntEqual[F[_]: Bind](implicit e: Equal[List[F[(Int, Int)]]]): Equal[StateT[Int, F, Int]] =
    e.contramap((s: StateT[Int, F, Int]) => stateTestInts.map(s.run(_)))

  checkAll(equal.laws[StateTListInt[Int]])
  checkAll(bindRec.laws[StateTListInt])
  checkAll(monad.laws[StateTListInt])
  checkAll(profunctor.laws[IStateTList])
  checkAll(monadTrans.laws[StateT[Int, *[_], *], List])
  checkAll(monadError.laws[StateT[Int, Either[Int, *], *], Int])

  checkAll {
    // Not sure why this is needed explicitly
    val am: Arbitrary[State[Int, Int]]        = implicitly
    val af: Arbitrary[State[Int, Int] => Int] = implicitly
    val eq: Equal[State[Int, Int]]            = implicitly
    comonad.laws[State[Int, *]](implicitly, am, af, eq)
  }

  object instances {
    def functor[S, F[_] : Applicative] = Functor[StateT[S, F, *]]
    def plus[S1, S2, F[_]: Monad: Plus] = Plus[IndexedStateT[S1, S2, F, *]]
    def bindRec[S, F[_] : Monad : BindRec] = BindRec[StateT[S, F, *]]
    def monadState[S, F[_] : Monad] = MonadState[StateT[S, F, *], S]
    def monadPlus[S, F[_]: MonadPlus] = MonadPlus[StateT[S, F, *]]
    def decidable[S, F[_] : Decidable: Bind] = Decidable[StateT[S, F, *]]
    def divisible[S, F[_] : Divisible: Bind] = Divisible[StateT[S, F, *]]

    // F = Id
    def functor[S] = Functor[State[S, *]]
    def monadState[S] = MonadState[State[S, *], S]

    // checking absence of ambiguity
    def functor[S, F[_] : Monad] = Functor[StateT[S, F, *]]
    def plus[S, F[_]: MonadPlus] = Plus[StateT[S, F, *]]
    def divisible[S, F[_] : Decidable: Bind] = Divisible[StateT[S, F, *]]
  }

  "monadState.point" in {
    instances.monadState[Boolean].point(42).run(true) must_===((true, 42))
  }

  "monadState.state" in {
    instances.monadState[Int].state(i => (i+1, i%2 == 0)).run(42) must_===((43, true))
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
    val a = StateT[Int, List, Boolean](s => List((s, false)))
    val b = StateT[Int, List, Boolean](s => List((s, true)))
    instances.monadPlus[Int, List].plus(a, b).run(0) must_===(List((0, false), (0, true)))
  }

  "StateT can be trampolined without stack overflow" in {
    import scalaz.Free._
    val result = (0 to 4000).toList.map(i => StateT[Int, Trampoline, Int]((ii:Int) => Trampoline.done((i,i))))
      .foldLeft(StateT((s:Int) => Trampoline.done((s,s))))( (a,b) => a.flatMap(_ => b))
    4000 must_=== result(0).run._1
  }

  "State can be run without stack overflow" in {
    val st = (0 to 10000).foldLeft(State[Int, Int](s => (s, s)))((s, i) => s.flatMap(_ => State((_: Int) => (i, i))))
    st.evalRec(0) must_=== (10000)
  }

  "State with nasty flatMaps can be run without stack overflow" in {
    def nasty: State[Int, Int] =
      State[Int, Int](n => (n-1, n-1)).flatMap(i =>
        if(i > 0) nasty
        else State[Int, Int](n => (n, n))
      )

    nasty.evalRec(10000) must_=== (0)
  }

  "iterated zoom on trampolined StateT is stack-safe" in {
    import scalaz.Free._
    val l: Lens[Int, Int] = Lens.lensId[Int]
    val st = (0 to 10000).foldLeft(StateT[Int, Trampoline, Int](s => Trampoline.done((s, s))))((s, _) => s.zoom(l))
    st.eval(5).run must_=== (5)
  }

  "iterated zoom on State is stack-safe" in {
    val l: Lens[Int, Int] = Lens.lensId[Int]
    val st = (0 to 10000).foldLeft(State[Int, Int](s => (s, s)))((s, _) => s.zoom(l))
    st.evalRec(5) must_=== (5)
  }

  "tailrecM is stack-safe, even when the given function returns an ugly StateT" in {
    def go(n: Int): State[Int, Int \/ Int] =
      (1 to n).foldLeft(State[Int, Int](s => (s, 0)))((s, i) => s.flatMap(_ => State[Int, Int](t => (t, i)))).map(\/.right)

    StateT.stateTBindRec[Int, Id].tailrecM(10000)(go).evalRec(0) must_=== (10000)
  }
}
