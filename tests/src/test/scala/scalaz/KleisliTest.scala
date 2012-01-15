package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.{Gen, Arbitrary}

class KleisliTest extends Spec {

  type KleisliOpt[A, B] = Kleisli[Option, A, B]
  type KleisliOptInt[B] = KleisliOpt[Int, B]

  implicit def Function1IntOptInt[A](implicit A: Arbitrary[Option[Int]]): Arbitrary[Int => Option[Int]] =
    Arbitrary(Gen.frequency[Int => Option[Int]](
      (1, Gen.value((x: Int) => Some(x))),
      (1, Gen.value((x: Int) => Some(x + 1))),
      (3, A.arbitrary.map(a => (_: Int) => a))
    ))

  implicit def KleisliEqual[M[_]](implicit M: Equal[M[Int]]): Equal[Kleisli[M, Int, Int]] = new Equal[Kleisli[M, Int, Int]] {
    def equal(a1: Kleisli[M, Int, Int], a2: Kleisli[M, Int, Int]): Boolean = {
      val mb1: M[Int] = a1.run(0)
      val mb2: M[Int] = a2.run(0)
      M.equal(mb1, mb2)
    }
  }

  checkAll(plus.laws[KleisliOptInt])
  checkAll(monoid.laws[KleisliOptInt[Int]])
  checkAll(monad.laws[KleisliOptInt])
  checkAll(category.laws[KleisliOpt])

  object instances {
    def semigroup[F[_], A, B](implicit FB: Semigroup[F[B]]) = Semigroup[Kleisli[F, A, B]]
    def monoid[F[_], A, B](implicit FB: Monoid[F[B]]) = Monoid[Kleisli[F, A, B]]
    def functor[F[_] : Functor, A] = Functor[({type f[a] = Kleisli[F, A, a]})#f]
    def apply[F[_] : Apply, A] = Apply[({type f[a] = Kleisli[F, A, a]})#f]
    def pointed[F[_] : Pointed, A] = Pointed[({type f[a] = Kleisli[F, A, a]})#f]
    def plus[F[_] : Plus, A] = Plus[({type f[a] = Kleisli[F, A, a]})#f]
    def empty[F[_] : Empty, A] = Empty[({type f[a] = Kleisli[F, A, a]})#f]
    def monadReader[F[_] : Monad, A] = MonadReader[({type f[s, a] = Kleisli[F, s, a]})#f, A]

    def arrId[F[_]: Pointed, A] = ArrId[({type λ[α, β]=Kleisli[F, α, β]})#λ]
    def category[F[_]: Monad, A] = ArrId[({type λ[α, β]=Kleisli[F, α, β]})#λ]
    def arrow[F[_]: Monad, A] = Arrow[({type λ[α, β]=Kleisli[F, α, β]})#λ]

    // F = Id
    def readerFunctor[A] = Functor[({type λ[α]=Reader[A, α]})#λ]
    def readerApply[A] = Apply[({type λ[α]=Reader[A, α]})#λ]
    def readerPointed[A] = Pointed[({type λ[α]=Reader[A, α]})#λ]
    def readerMonadReader[A] = MonadReader[({type f[s, a] = Reader[s, a]})#f, A]
    def readerArrId[A] = ArrId[Reader]
    def readerCategory[A] = ArrId[Reader]
    def readerArrow[A] = Arrow[Reader]
  }
}