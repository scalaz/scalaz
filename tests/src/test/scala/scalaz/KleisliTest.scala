package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.{Gen, Arbitrary}
import Id._

class KleisliTest extends Spec {

  type KleisliOpt[A, B] = Kleisli[Option, A, B]
  type KleisliOptInt[B] = KleisliOpt[Int, B]

  implicit def Function1IntOptInt[A](implicit A: Arbitrary[Option[Int]]): Arbitrary[Int => Option[Int]] =
    Arbitrary(Gen.frequency[Int => Option[Int]](
      (1, Gen.value((x: Int) => Some(x))),
      (1, Gen.value((x: Int) => Some(x + 1))),
      (3, A.arbitrary.map(a => (_: Int) => a))
    ))

  implicit def KleisliEqual[M[+_]](implicit M: Equal[M[Int]]): Equal[Kleisli[M, Int, Int]] = new Equal[Kleisli[M, Int, Int]] {
    def equal(a1: Kleisli[M, Int, Int], a2: Kleisli[M, Int, Int]): Boolean = {
      val mb1: M[Int] = a1.run(0)
      val mb2: M[Int] = a2.run(0)
      M.equal(mb1, mb2)
    }
  }
  
  "mapK" ! prop {
    (f: Int => Option[Int], a: Int) => 
      Kleisli(f).mapK(_.toList.map(_.toString)).run(a)  must be_===(f(a).toList.map(_.toString))
  }

  checkAll(monoid.laws[KleisliOptInt[Int]])
  checkAll(monadPlus.strongLaws[KleisliOptInt])
  checkAll(category.laws[KleisliOpt])

  object instances {
    def semigroup[F[+_], A, B](implicit FB: Semigroup[F[B]]) = Semigroup[Kleisli[F, A, B]]
    def monoid[F[+_], A, B](implicit FB: Monoid[F[B]]) = Monoid[Kleisli[F, A, B]]
    def functor[F[+_] : Functor, A] = Functor[({type f[a] = Kleisli[F, A, a]})#f]
    def apply[F[+_] : Apply, A] = Apply[({type f[a] = Kleisli[F, A, a]})#f]
    def plus[F[+_] : Plus, A] = Plus[({type f[a] = Kleisli[F, A, a]})#f]
    def empty[F[+_] : PlusEmpty, A] = PlusEmpty[({type f[a] = Kleisli[F, A, a]})#f]
    def monadReader[F[+_] : Monad, A] = MonadReader[({type f[s, a] = Kleisli[F, s, a]})#f, A]

    def category[F[+_]: Monad, A] = Category[({type λ[α, β]=Kleisli[F, α, β]})#λ]
    def arrow[F[+_]: Monad, A] = Arrow[({type λ[α, β]=Kleisli[F, α, β]})#λ]

    // checking absence of ambiguity
    def semigroup[F[+_], A, B](implicit FB: Monoid[F[B]]) = Semigroup[Kleisli[F, A, B]]
    def functor[F[+_] : Monad, A] = Functor[({type f[a] = Kleisli[F, A, a]})#f]
    def apply[F[+_] : Monad, A] = Apply[({type f[a] = Kleisli[F, A, a]})#f]
    def plus[F[+_] : PlusEmpty, A] = Plus[({type f[a] = Kleisli[F, A, a]})#f]
    def empty[F[+_] : MonadPlus, A] = PlusEmpty[({type f[a] = Kleisli[F, A, a]})#f]

    object reader {
      // F = Id
      def readerFunctor[A] = Functor[({type λ[α] = Reader[A, α]})#λ]
      def readerApply[A] = Apply[({type λ[α] = Reader[A, α]})#λ]
      def readerMonadReader[A] = MonadReader[({type f[s, a] = Reader[s, a]})#f, A]
      def readerCategory = Ca1tegory[Reader]
      def readerArrow = Arrow[Reader]

      // Sigh, more tests needed, see http://stackoverflow.com/questions/11913128/scalaz-7-why-using-type-alias-results-in-ambigous-typeclass-resolution-for-rea
      trait X
      type ReaderX[A] = Reader[X, A]
      def readerXFunctor = Functor[ReaderX]
      def readerXApply = Apply[ReaderX]
    }
  }
}
