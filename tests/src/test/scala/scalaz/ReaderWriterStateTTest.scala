package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._

object ReaderWriterStateTTest extends SpecLite {
  type RWSOptInt[A] = RWST[Option, Int, Int, Int, A]

  implicit val RWSOptIntEqual = new Equal[RWSOptInt[Int]] {
    def equal(a1: RWSOptInt[Int], a2: RWSOptInt[Int]) = a1.run(0, 0) == a2.run(0, 0)
  }

  checkAll(bindRec.laws[RWSOptInt])
  checkAll(monadPlus.strongLaws[RWSOptInt])

  "ReaderWriterStateT can be trampolined without stack overflow" in {
    import scalaz.Free._
    val result = (0 to 10000).toList.map(ii => ReaderWriterStateT[Trampoline, Unit, String, Int, Int]((_, i: Int) => Trampoline.done(("", i, ii))))
      .foldLeft(ReaderWriterStateT[Trampoline, Unit, String, Int, Int]((_, i: Int) => Trampoline.done(("", i, i))))( (a, b) => a.flatMap(_ => b))
    10000 must_=== result.run((),0).run._3
  }

  object instances {
    def functor[F[_]: Functor, R, W, S] = Functor[RWST[F, R, W, S, ?]]
    def plus[F[_]: Plus, R, W, S1, S2] = Plus[IRWST[F, R, W, S1, S2, ?]]
    def plusEmpty[F[_]: PlusEmpty, R, W, S1, S2] = PlusEmpty[IRWST[F, R, W, S1, S2, ?]]
    def bindRec[F[_]: BindRec : Monad, R, W: Semigroup, S] = BindRec[RWST[F, R, W, S, ?]]
    def monad[F[_]: Monad, R, W: Monoid, S] = Monad[RWST[F, R, W, S, ?]]
    def monadPlus[F[_]: MonadPlus, R, W: Monoid, S] = MonadPlus[RWST[F, R, W, S, ?]]
    def bind[F[_]: Bind, R, W: Semigroup, S] = Bind[RWST[F, R, W, S, ?]]
    def monadReader[F[_]: Monad, R, W: Monoid, S] = MonadReader[RWST[F, R, W, S, ?], R]
    def monadState[F[_]: Monad, R, W: Monoid, S] = MonadState[RWST[F, R, W, S, ?], S]
    def monadTrans[R, W: Monoid, S] = MonadTrans[λ[(f[_], α) => RWST[f, R, W, S, α]]]
    // checking absence of ambiguity
    def functor[F[_]: Monad, R, W: Monoid, S] = Functor[RWST[F, R, W, S, ?]]
    def functor[F[_]: Bind, R, W: Semigroup, S] = Functor[RWST[F, R, W, S, ?]]
    def functor[F[_]: MonadPlus, R, W: Monoid, S] = Functor[RWST[F, R, W, S, ?]]
    def plus[F[_]: PlusEmpty, R, W, S] = Plus[RWST[F, R, W, S, ?]]
    def plus[F[_]: MonadPlus, R, W, S] = Plus[RWST[F, R, W, S, ?]]
    def plusEmpty[F[_]: MonadPlus, R, W, S] = PlusEmpty[RWST[F, R, W, S, ?]]
    def bind[F[_]: Monad, R, W: Monoid, S] = Bind[RWST[F, R, W, S, ?]]
    def bind[F[_]: MonadPlus, R, W: Monoid, S] = Bind[RWST[F, R, W, S, ?]]
    def monad[F[_]: MonadPlus, R, W: Monoid, S] = Monad[RWST[F, R, W, S, ?]]
  }
}
