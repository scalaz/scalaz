package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import org.scalacheck.Prop.forAll

object KleisliTest extends SpecLite {

  type KleisliOpt[A, B] = Kleisli[Option, A, B]
  type KleisliOptInt[B] = KleisliOpt[Int, B]
  type IntOr[A] = Int \/ A
  type KleisliEither[A] = Kleisli[IntOr, Int, A]

  implicit def KleisliEqual[M[_]](implicit M: Equal[M[Int]]): Equal[Kleisli[M, Int, Int]] = new Equal[Kleisli[M, Int, Int]] {
    def equal(a1: Kleisli[M, Int, Int], a2: Kleisli[M, Int, Int]): Boolean = {
      val mb1: M[Int] = a1.run(0)
      val mb2: M[Int] = a2.run(0)
      M.equal(mb1, mb2)
    }
  }

  "mapK" ! forAll {
    (f: Int => Option[Int], a: Int) =>
      Kleisli(f).mapK(_.toList.map(_.toString)).run(a)  must_===(f(a).toList.map(_.toString))
  }

  checkAll(monoid.laws[KleisliOptInt[Int]])
  checkAll(bindRec.laws[KleisliOptInt])
  checkAll(monadPlus.strongLaws[KleisliOptInt])
  checkAll(monadError.laws[KleisliEither, Int])
  checkAll(zip.laws[KleisliOptInt])
  checkAll(category.laws[KleisliOpt])

  object instances {
    def semigroup[F[_], A, B](implicit FB: Semigroup[F[B]]) = Semigroup[Kleisli[F, A, B]]
    def monoid[F[_], A, B](implicit FB: Monoid[F[B]]) = Monoid[Kleisli[F, A, B]]
    def functor[F[_] : Functor, A] = Functor[Kleisli[F, A, ?]]
    def apply[F[_] : Apply, A] = Apply[Kleisli[F, A, ?]]
    def applicative[F[_] : Applicative, A] = Applicative[Kleisli[F, A, ?]]
    def bind[F[_] : Bind , A] = Bind[Kleisli[F, A, ?]]
    def plus[F[_] : Plus, A] = Plus[Kleisli[F, A, ?]]
    def empty[F[_] : PlusEmpty, A] = PlusEmpty[Kleisli[F, A, ?]]
    def bindRec[F[_] : BindRec, A] = BindRec[Kleisli[F, A, ?]]
    def monadReader[F[_] : Monad, A] = MonadReader[Kleisli[F, A, ?], A]
    def zip[F[_] : Zip, A] = Zip[Kleisli[F, A, ?]]

    def profunctor[F[_]: Functor] = Profunctor[Kleisli[F, ?, ?]]
    def strong[F[_]: Functor] = Strong[Kleisli[F, ?, ?]]
    def proChoice[F[_]: Applicative] = ProChoice[Kleisli[F, ?, ?]]
    def compose[F[_]: Bind] = Compose[Kleisli[F, ?, ?]]
    def category[F[_]: Monad] = Category[Kleisli[F, ?, ?]]
    def arrow[F[_]: Monad] = Arrow[Kleisli[F, ?, ?]]
    def choice[F[_]: Monad] = Choice[Kleisli[F, ?, ?]]

    // checking absence of ambiguity
    def semigroup[F[_], A, B](implicit FB: Monoid[F[B]]) = Semigroup[Kleisli[F, A, B]]
    def functor[F[_] : Monad, A] = Functor[Kleisli[F, A, ?]]
    def functor[F[_] : Bind, A] = Functor[Kleisli[F, A, ?]]
    def functor[F[_] : Apply, A] = Functor[Kleisli[F, A, ?]]
    def functor[F[_] : Applicative, A] = Functor[Kleisli[F, A, ?]]
    def functor[F[_] : BindRec, A] = Functor[Kleisli[F, A, ?]]
    def functor[F[_] : Monad: BindRec, A] = Functor[Kleisli[F, A, ?]]
    def functor[F[_] : Applicative: BindRec, A] = Functor[Kleisli[F, A, ?]]
    def functor[F[_] : ApplicativePlus: BindRec, A] = Functor[Kleisli[F, A, ?]]
    def apply[F[_] : Monad, A] = Apply[Kleisli[F, A, ?]]
    def apply[F[_] : Bind, A] = Apply[Kleisli[F, A, ?]]
    def apply[F[_] : BindRec, A] = Apply[Kleisli[F, A, ?]]
    def apply[F[_] : Applicative, A] = Apply[Kleisli[F, A, ?]]
    def apply[F[_] : Monad: BindRec, A] = Apply[Kleisli[F, A, ?]]
    def apply[F[_] : Applicative: BindRec, A] = Apply[Kleisli[F, A, ?]]
    def apply[F[_] : ApplicativePlus: BindRec, A] = Apply[Kleisli[F, A, ?]]
    def applicative[F[_] : Monad, A] = Applicative[Kleisli[F, A, ?]]
    def bind[F[_] : BindRec, A] = Bind[Kleisli[F, A, ?]]
    def bind[F[_] : Monad: BindRec, A] = Bind[Kleisli[F, A, ?]]
    def plus[F[_] : PlusEmpty, A] = Plus[Kleisli[F, A, ?]]
    def empty[F[_] : MonadPlus, A] = PlusEmpty[Kleisli[F, A, ?]]
    def profunctor[F[_]: Applicative] = Profunctor[Kleisli[F, ?, ?]]
    def profunctor[F[_]: Monad] = Profunctor[Kleisli[F, ?, ?]]
    def strong[F[_]: Monad] = Strong[Kleisli[F, ?, ?]]
    def proChoice[F[_]: Monad] = ProChoice[Kleisli[F, ?, ?]]
    def compose[F[_]: Monad] = Compose[Kleisli[F, ?, ?]]

    object reader {
      // F = Id
      def readerFunctor[A] = Functor[Reader[A, ?]]
      def readerApply[A] = Apply[Reader[A, ?]]
      def readerMonadReader[A] = MonadReader[Reader[A, ?], A]
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

    object strong{
      import syntax.strong._
      k1.first
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
