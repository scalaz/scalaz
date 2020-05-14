package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._

object ReaderWriterStateTTest extends SpecLite {
  type RWSOptInt[A] = RWST[Int, Int, Int, Option, A]

  implicit val RWSOptIntEqual: Equal[RWSOptInt[Int]] = new Equal[RWSOptInt[Int]] {
    def equal(a1: RWSOptInt[Int], a2: RWSOptInt[Int]) = a1.run(0, 0) == a2.run(0, 0)
  }

  private[this] implicit val RWSEitherIntEqual: Equal[RWST[Int, Int, Int, Either[Int, *], Int]] =
    Equal.equal[RWST[Int, Int, Int, Either[Int, *], Int]] { (a1, a2) =>
      (1 to 5).forall{ x =>
        (1 to 5).forall{ y =>
          Equal[Either[Int, (Int, Int, Int)]].equal(a1.run(x, y), a2.run(x, y))
        }
      }
    }

  checkAll(bindRec.laws[RWSOptInt])
  checkAll(monadPlus.strongLaws[RWSOptInt])
  checkAll(monadTrans.laws[ReaderWriterStateT[Int, Int, Int, *[_], *], Option])
  checkAll(monadError.laws[RWST[Int, Int, Int, Either[Int, *], *], Int])

  "ReaderWriterStateT can be trampolined without stack overflow" in {
    import scalaz.Free._
    val result = (0 to 10000).toList.map(ii => ReaderWriterStateT[Unit, String, Int, Trampoline, Int]((_, i: Int) => Trampoline.done(("", i, ii))))
      .foldLeft(ReaderWriterStateT[Unit, String, Int, Trampoline, Int]((_, i: Int) => Trampoline.done(("", i, i))))( (a, b) => a.flatMap(_ => b))
    10000 must_=== result.run((),0).run._3
  }

  object instances {
    def functor[R, W, S, F[_]: Functor] = Functor[RWST[R, W, S, F, *]]
    def plus[R, W, S1, S2, F[_]: Plus] = Plus[IRWST[R, W, S1, S2, F, *]]
    def plusEmpty[R, W, S1, S2, F[_]: PlusEmpty] = PlusEmpty[IRWST[R, W, S1, S2, F, *]]
    def bindRec[R, W: Semigroup, S, F[_]: BindRec : Monad] = BindRec[RWST[R, W, S, F, *]]
    def monad[R, W: Monoid, S, F[_]: Monad] = Monad[RWST[R, W, S, F, *]]
    def monadPlus[R, W: Monoid, S, F[_]: MonadPlus] = MonadPlus[RWST[R, W, S, F, *]]
    def bind[R, W: Semigroup, S, F[_]: Bind] = Bind[RWST[R, W, S, F, *]]
    def monadReader[R, W: Monoid, S, F[_]: Monad] = MonadReader[RWST[R, W, S, F, *], R]
    def monadState[R, W: Monoid, S, F[_]: Monad] = MonadState[RWST[R, W, S, F, *], S]
    def monadTrans[R, W: Monoid, S] = MonadTrans[λ[(f[_], α) => RWST[R, W, S, f, α]]]
    // checking absence of ambiguity
    def functor[R, W: Monoid, S, F[_]: Monad] = Functor[RWST[R, W, S, F, *]]
    def functor[R, W: Semigroup, S, F[_]: Bind] = Functor[RWST[R, W, S, F, *]]
    def functor[R, W: Monoid, S, F[_]: MonadPlus] = Functor[RWST[R, W, S, F, *]]
    def plus[R, W, S, F[_]: PlusEmpty] = Plus[RWST[R, W, S, F, *]]
    def plus[R, W, S, F[_]: MonadPlus] = Plus[RWST[R, W, S, F, *]]
    def plusEmpty[R, W, S, F[_]: MonadPlus] = PlusEmpty[RWST[R, W, S, F, *]]
    def bind[R, W: Monoid, S, F[_]: Monad] = Bind[RWST[R, W, S, F, *]]
    def bind[R, W: Monoid, S, F[_]: MonadPlus] = Bind[RWST[R, W, S, F, *]]
    def monad[R, W: Monoid, S, F[_]: MonadPlus] = Monad[RWST[R, W, S, F, *]]
  }
}
