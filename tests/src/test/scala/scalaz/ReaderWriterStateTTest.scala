package scalaz

import scalaz.scalacheck.ScalazProperties._
import std.AllInstances._
import org.scalacheck.{Gen, Arbitrary}
import org.scalacheck.Prop.forAll

object ReaderWriterStateTTest extends SpecLite {
  type RWSOptInt[A] = RWST[Option, Int, Int, Int, A]
  implicit val RWSOptIntArb = Arbitrary(Gen.oneOf[RWSOptInt[Int]](
    Gen.value(RWST[Option, Int, Int, Int, Int]((r: Int, s: Int) => None)),
    Gen.value(RWST[Option, Int, Int, Int, Int]((r: Int, s: Int) => Some((0, 0, 0)))),
    Gen.value(RWST[Option, Int, Int, Int, Int]((r: Int, s: Int) => Some((r, r, r)))),
    Gen.value(RWST[Option, Int, Int, Int, Int]((r: Int, s: Int) => Some((s, s, s))))
  ))
  implicit val RWSOptIntIntArb = Arbitrary(Gen.oneOf[RWSOptInt[Int => Int]](
    Gen.value(RWST[Option, Int, Int, Int, Int => Int]((r: Int, s: Int) => None)),
    Gen.value(RWST[Option, Int, Int, Int, Int => Int]((r: Int, s: Int) => Some((0, x => 0, 0)))),
    Gen.value(RWST[Option, Int, Int, Int, Int => Int]((r: Int, s: Int) => Some((r, x => r, r)))),
    Gen.value(RWST[Option, Int, Int, Int, Int => Int]((r: Int, s: Int) => Some((s, x => s, s)))),
    Gen.value(RWST[Option, Int, Int, Int, Int => Int]((r: Int, s: Int) => Some((s, x => x, s))))
  ))

  implicit val RWSOptIntEqual = new Equal[RWSOptInt[Int]] {
    def equal(a1: RWSOptInt[Int], a2: RWSOptInt[Int]) = a1.run(0, 0) == a2.run(0, 0)
  }

  checkAll(monadPlus.strongLaws[RWSOptInt])

  object instances {
    def functor[F[_]: Functor, R, W, S] = Functor[({type λ[α]=RWST[F, R, W, S, α]})#λ]
    def plus[F[_]: Plus, R, W, S1, S2] = Plus[({type λ[α]=IRWST[F, R, W, S1, S2, α]})#λ]
    def plusEmpty[F[_]: PlusEmpty, R, W, S1, S2] = PlusEmpty[({type λ[α]=IRWST[F, R, W, S1, S2, α]})#λ]
    def monad[F[_]: Monad, R, W: Monoid, S] = Monad[({type λ[α]=RWST[F, R, W, S, α]})#λ]
    def monadPlus[F[_]: MonadPlus, R, W: Monoid, S] = MonadPlus[({type λ[α]=RWST[F, R, W, S, α]})#λ]
    def monadReader[F[_]: Monad, R, W: Monoid, S] = MonadReader[({type λ[r, α]=RWST[F, r, W, S, α]})#λ, R]
    def monadState[F[_]: Monad, R, W: Monoid, S] = MonadState[({type λ[s, α]=RWST[F, R, W, s, α]})#λ, S]
    def monadTrans[R, W: Monoid, S] = MonadTrans[({type λ[f[_], α]=RWST[f, R, W, S, α]})#λ]
    // checking absence of ambiguity
    def functor[F[_]: Monad, R, W: Monoid, S] = Functor[({type λ[α]=RWST[F, R, W, S, α]})#λ]
    def functor[F[_]: MonadPlus, R, W: Monoid, S] = Functor[({type λ[α]=RWST[F, R, W, S, α]})#λ]
    def plus[F[_]: PlusEmpty, R, W, S] = Plus[({type λ[α]=RWST[F, R, W, S, α]})#λ]
    def plus[F[_]: MonadPlus, R, W, S] = Plus[({type λ[α]=RWST[F, R, W, S, α]})#λ]
    def plusEmpty[F[_]: MonadPlus, R, W, S] = PlusEmpty[({type λ[α]=RWST[F, R, W, S, α]})#λ]
    def monad[F[_]: MonadPlus, R, W: Monoid, S] = Monad[({type λ[α]=RWST[F, R, W, S, α]})#λ]
  }
}
