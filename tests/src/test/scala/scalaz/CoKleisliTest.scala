package scalaz

import scalaz.scalacheck.ScalazProperties._
import org.scalacheck.{Arbitrary, Gen}

object CokleisliTest extends SpecLite {

  implicit val cokleisliArb1: Arbitrary[Cokleisli[Option, Int, Int]] = {
    def arb(f: (Option[Int], Int, Int) => Int): Gen[Cokleisli[Option, Int, Int]] =
      for{
        a <- implicitly[Arbitrary[Int]].arbitrary
        b <- implicitly[Arbitrary[Int]].arbitrary
      } yield Cokleisli[Option, Int, Int](f(_, a, b))

    Arbitrary(Gen.oneOf(
      arb((_, n, _) => n),
      arb((a, b, c) => a getOrElse b),
      arb((a, b, c) => a.map(_ + b) getOrElse c),
      arb((a, b, c) => a.map(_ - b) getOrElse c)
    ))
  }

  implicit val cokleisliArb2: Arbitrary[Cokleisli[Option, Int, Int => Int]] = {
    def arb(f: (Option[Int], Int) => (Int => Int)): Gen[Cokleisli[Option, Int, Int => Int]] =
      implicitly[Arbitrary[Int]].arbitrary.map(a => Cokleisli[Option, Int, Int => Int](f(_, a)))

    Arbitrary(Gen.oneOf(
      arb((_, n) => Function.const(n)),
      arb((_, _) => x => x),
      arb((_, n) => _ + n),
      arb((_, n) => _ - n),
      arb((a, b) => a.map(_ + b) getOrElse _),
      arb((a, b) => a.map(_ - b) getOrElse _)
    ))
  }

  implicit val cokleisliEqual: Equal[Cokleisli[Option, Int, Int]] =
    Equal.equal{ (a, b) =>
      a(None) == b(None) && Iterator.fill(20)(util.Random.nextInt).map(Option(_)).forall(n => a(n) == b(n))
    }

  checkAll(bindRec.laws[Cokleisli[Option, Int, ?]])
  checkAll(bind.laws[Cokleisli[Option, Int, ?]])

  "compose" in {
    import std.AllInstances._

    val ck = Cokleisli((a: NonEmptyList[Int]) => a.size)
    val ck1 = ck compose ck
    val run: Int = ck1.run(NonEmptyList(0, 0))
    run must_===(2)
  }

  object instances {
    def bindRec[F[_], W] = BindRec[Cokleisli[F, W, ?]]
    def monad[F[_], W] = Monad[Cokleisli[F, W, ?]]
    def compose[F[_]](implicit F: Cobind[F]) = Compose[Cokleisli[F, ?, ?]]
    def profunctor[F[_]: Functor] = Profunctor[Cokleisli[F, ?, ?]]
    def arrow[F[_] : Comonad] = Arrow[Cokleisli[F, ?, ?]]
    def prochoice[F[_] : Comonad] = ProChoice[Cokleisli[F, ?, ?]]

    // checking absence of ambiguity
    def compose[F[_]](implicit F: Comonad[F]) = Compose[Cokleisli[F, ?, ?]]
    def profunctor[F[_]: Comonad] = Profunctor[Cokleisli[F, ?, ?]]
  }
}
