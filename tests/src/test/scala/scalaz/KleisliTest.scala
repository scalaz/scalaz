package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.{Prop, Gen, Arbitrary}
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Prop.forAll

object KleisliTest extends SpecLite {

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

  // Needed because scalac inference has trouble with \/
  implicit def KleisliDisjunctionArbitrary[E : Arbitrary, R : Arbitrary, A : Arbitrary]: Arbitrary[Kleisli[({type l[a] = E \/ a})#l, R, A]] =
    KleisliArbitrary[({type l[a] = E \/ a})#l, R, A]

  implicit def KleisliDisjunctionEqual[E : Equal] = KleisliEqual[({type l[a] = E \/ a})#l]

  "mapK" ! forAll {
    (f: Int => Option[Int], a: Int) =>
      Kleisli(f).mapK(_.toList.map(_.toString)).run(a)  must_===(f(a).toList.map(_.toString))
  }

  checkAll(monoid.laws[KleisliOptInt[Int]])
  checkAll(monadPlus.strongLaws[KleisliOptInt])
  checkAll(zip.laws[KleisliOptInt])
  checkAll(monadError.laws[({type x[E0, A] = Kleisli[({type y[a] = E0 \/ a})#y, Int, A]})#x, Int])
  checkAll(category.laws[KleisliOpt])

  object instances {
    def semigroup[F[_], A, B](implicit FB: Semigroup[F[B]]) = Semigroup[Kleisli[F, A, B]]
    def monoid[F[_], A, B](implicit FB: Monoid[F[B]]) = Monoid[Kleisli[F, A, B]]
    def functor[F[_] : Functor, A] = Functor[({type f[a] = Kleisli[F, A, a]})#f]
    def apply[F[_] : Apply, A] = Apply[({type f[a] = Kleisli[F, A, a]})#f]
    def applicative[F[_] : Applicative, A] = Applicative[({type f[a] = Kleisli[F, A, a]})#f]
    def plus[F[_] : Plus, A] = Plus[({type f[a] = Kleisli[F, A, a]})#f]
    def empty[F[_] : PlusEmpty, A] = PlusEmpty[({type f[a] = Kleisli[F, A, a]})#f]
    def monadReader[F[_] : Monad, A] = MonadReader[({type f[s, a] = Kleisli[F, s, a]})#f, A]
    def zip[F[_] : Zip, A] = Zip[({type f[a] = Kleisli[F, A, a]})#f]

    def profunctor[F[_]: Functor] = Profunctor[({type λ[α, β]=Kleisli[F, α, β]})#λ]
    def proChoice[F[_]: Applicative] = ProChoice[({type λ[α, β]=Kleisli[F, α, β]})#λ]
    def compose[F[_]: Bind] = Compose[({type λ[α, β]=Kleisli[F, α, β]})#λ]
    def category[F[_]: Monad] = Category[({type λ[α, β]=Kleisli[F, α, β]})#λ]
    def arrow[F[_]: Monad] = Arrow[({type λ[α, β]=Kleisli[F, α, β]})#λ]
    def choice[F[_]: Monad] = Choice[({type λ[α, β]=Kleisli[F, α, β]})#λ]

    // checking absence of ambiguity
    def semigroup[F[_], A, B](implicit FB: Monoid[F[B]]) = Semigroup[Kleisli[F, A, B]]
    def functor[F[_] : Monad, A] = Functor[({type f[a] = Kleisli[F, A, a]})#f]
    def functor[F[_] : Apply, A] = Functor[({type f[a] = Kleisli[F, A, a]})#f]
    def functor[F[_] : Applicative, A] = Functor[({type f[a] = Kleisli[F, A, a]})#f]
    def apply[F[_] : Monad, A] = Apply[({type f[a] = Kleisli[F, A, a]})#f]
    def apply[F[_] : Applicative, A] = Apply[({type f[a] = Kleisli[F, A, a]})#f]
    def applicative[F[_] : Monad, A] = Applicative[({type f[a] = Kleisli[F, A, a]})#f]
    def plus[F[_] : PlusEmpty, A] = Plus[({type f[a] = Kleisli[F, A, a]})#f]
    def empty[F[_] : MonadPlus, A] = PlusEmpty[({type f[a] = Kleisli[F, A, a]})#f]
    def profunctor[F[_]: Applicative] = Profunctor[({type λ[α, β]=Kleisli[F, α, β]})#λ]
    def profunctor[F[_]: Monad] = Profunctor[({type λ[α, β]=Kleisli[F, α, β]})#λ]
    def proChoice[F[_]: Monad] = ProChoice[({type λ[α, β]=Kleisli[F, α, β]})#λ]
    def compose[F[_]: Monad] = Compose[({type λ[α, β]=Kleisli[F, α, β]})#λ]

    object reader {
      // F = Id
      def readerFunctor[A] = Functor[({type λ[α] = Reader[A, α]})#λ]
      def readerApply[A] = Apply[({type λ[α] = Reader[A, α]})#λ]
      def readerMonadReader[A] = MonadReader[({type f[s, a] = Reader[s, a]})#f, A]
      def readerCategory = Category[Reader]
      def readerArrow = Arrow[Reader]

      // Sigh, more tests needed, see http://stackoverflow.com/questions/11913128/scalaz-7-why-using-type-alias-results-in-ambigous-typeclass-resolution-for-rea
      trait X
      type ReaderX[A] = Reader[X, A]
      def readerXFunctor = Functor[ReaderX]
      def readerXApply = Apply[ReaderX]
    }
  }

  object `FromKleisliLike inference` {
    val k1: Kleisli[Option, Int, String] = Kleisli(i => Option("a"))
    val k2: Kleisli[Option, String, Int] = Kleisli(s => Option(1))

    object compose{
      import syntax.compose._
      k1 >>> k2
    }

    object choice{
      import syntax.choice._
      k1 ||| k1
    }

    object split{
      import syntax.split._
      k1 -*- k1
    }

    object profunctor{
      import syntax.profunctor._
      k1.mapsnd(x => x)
    }

    object proChoice{
      import syntax.proChoice._
      k1.proleft
    }

    object arrow{
      import syntax.arrow._
      k1 *** k1
    }

    object all{
      import syntax.all._

      k1 >>> k2
      k1 ||| k1
      k1 -*- k1
      k1.mapsnd(x => x)
      k1 *** k1
    }
  }

  "Catchable[Kleisli]" should {

    import effect.IO

    type F[A] = Kleisli[IO, Int, A]
    val C = Catchable[F]
    val err = new Error("oh noes")
    val bad = C.fail[Int](err)

    "throw exceptions captured via fail()" in {
      try {
        bad.run(1).unsafePerformIO
        fail("should have thrown")
      } catch {
        case t: Throwable => t must_== err
      }
    }

    "catch exceptions captured via fail()" in {
      C.attempt(bad).run(1).unsafePerformIO must_== -\/(err)
    }

    "catch ambient exceptions (1/2)" in {
      C.attempt(Kleisli(_ => IO[Int](throw err))).run(1).unsafePerformIO must_== -\/(err)
    }

    "catch ambient exceptions (2/2)" in {
      C.attempt(Kleisli(_ => throw err)).run(1).unsafePerformIO must_== -\/(err)
    }

    "properly handle success" in {
      C.attempt(Kleisli(n => IO(n + 2))).run(1).unsafePerformIO must_== \/-(3)
    }

  }

}
