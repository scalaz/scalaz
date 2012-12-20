package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary
import scalaz.scalacheck.ScalazArbitrary.{stateTArb => _, indexedStateTArb => _, _}
import std.AllInstances._
import org.scalacheck.{Gen, Arbitrary}

class ReaderWriterStateTTest extends Spec {
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

  checkAll(monad.laws[RWSOptInt])

  object instances {
    def functor[F[+_]: Functor, R, W, S] = Functor[({type λ[+α]=RWST[F, R, W, S, α]})#λ]
    def monad[F[+_]: Monad, R, W: Monoid, S] = Monad[({type λ[+α]=RWST[F, R, W, S, α]})#λ]
    def monadReader[F[+_]: Monad, R, W: Monoid, S] = MonadReader[({type λ[-r, +α]=RWST[F, r, W, S, α]})#λ, R]
    def monadState[F[+_]: Monad, R, W: Monoid, S] = MonadState[({type λ[s, +α]=RWST[F, R, W, s, α]})#λ, S]
    def monadTrans[R, W: Monoid, S] = MonadTrans[({type λ[f[+_], α]=RWST[f, R, W, S, α]})#λ]
    // checking absence of ambiguity
    def functor[F[+_]: Monad, R, W: Monoid, S] = Functor[({type λ[+α]=RWST[F, R, W, S, α]})#λ]
  }
}
