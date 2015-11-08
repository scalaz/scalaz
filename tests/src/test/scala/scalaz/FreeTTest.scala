package scalaz

import org.scalacheck.{Arbitrary, Gen}
import std.AllInstances._
import FreeT._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalaCheckBinding._

case class FreeTListOption[A](f: FreeT[List, Option, A])

object FreeTListOption {
  implicit def freeTListOptionMonad = new Monad[FreeTListOption] with BindRec[FreeTListOption] {
    def point[A](a: => A): FreeTListOption[A] =
      FreeTListOption(Monad[FreeT[List, Option, ?]].point(a))

    def bind[A, B](fa: FreeTListOption[A])(f: A => FreeTListOption[B]): FreeTListOption[B] =
      FreeTListOption(Monad[FreeT[List, Option, ?]].bind(fa.f) { a => f(a).f })

    def tailrecM[A, B](f: A => FreeTListOption[A \/ B])(a: A): FreeTListOption[B] =
      FreeTListOption(BindRec[FreeT[List, Option, ?]].tailrecM((x: A) => f(x).f)(a))
  }

  implicit def freeTListOptionArb[A](implicit A: Arbitrary[A]): Arbitrary[FreeTListOption[A]] =
    Arbitrary(FreeTTest.freeTGen[List, Option, A](
      Gen.choose(0, 2).flatMap(Gen.listOfN(_, freeTListOptionArb[A].arbitrary.map(_.f)))
    ).map(FreeTListOption.apply))

  implicit def freeTListOptionEq[A](implicit A: Equal[A]): Equal[FreeTListOption[A]] = new Equal[FreeTListOption[A]] {
    def equal(a: FreeTListOption[A], b: FreeTListOption[A]) = Equal[Option[A]].equal(a.f.runM(_.headOption), b.f.runM(_.headOption))
  }
}

object FreeTTest extends SpecLite {
  def freeTGen[F[_], G[_], A](g: Gen[F[FreeT[F, G, A]]])(implicit F: Functor[F], G: Applicative[G], A: Arbitrary[A]): Gen[FreeT[F, G, A]] =
    Gen.frequency(
      (1, Functor[Arbitrary].map(A)(FreeT.point[F, G, A](_)).arbitrary),
      (1, Functor[Arbitrary].map(Arbitrary(g))(FreeT.liftF[F, G, FreeT[F, G, A]](_).flatMap(x => x)).arbitrary)
    )
  "ListOption" should {
    checkAll(monad.laws[FreeTListOption])

    "not stack overflow with 50k binds" in {
      val expected = Applicative[FreeTListOption].point(())
      val result =
        BindRec[FreeTListOption].tailrecM((i: Int) =>
          if (i < 50000)
            Applicative[FreeTListOption].point(\/.left[Int, Unit](i + 1))
          else
            Applicative[FreeTListOption].point(\/.right[Int, Unit](()))
        )(0)

      Equal[FreeTListOption[Unit]].equal(expected, result)
    }
  }

  object instances {
    def monad[S[_]: Functor, F[_]: Applicative] = Monad[FreeT[S, F, ?]]

    // checking absence of ambiguity
    def functor[S[_]: Functor, F[_]: Functor] = Functor[FreeT[S, F, ?]]
  }
}
