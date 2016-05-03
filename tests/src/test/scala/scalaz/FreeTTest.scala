package scalaz

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import std.AllInstances._
import FreeT._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.syntax.bind._

case class FreeTListOption[A](f: FreeT[List, Option, A])

object FreeTListOption {
  implicit def freeTListOptionMonad = new MonadPlus[FreeTListOption] with Traverse[FreeTListOption] with BindRec[FreeTListOption] {
    def point[A](a: => A): FreeTListOption[A] =
      FreeTListOption(Monad[FreeT[List, Option, ?]].point(a))

    def bind[A, B](fa: FreeTListOption[A])(f: A => FreeTListOption[B]): FreeTListOption[B] =
      FreeTListOption(Monad[FreeT[List, Option, ?]].bind(fa.f) { a => f(a).f })

    def tailrecM[A, B](f: A => FreeTListOption[A \/ B])(a: A): FreeTListOption[B] =
      FreeTListOption(BindRec[FreeT[List, Option, ?]].tailrecM((x: A) => f(x).f)(a))

    def plus[A](a: FreeTListOption[A], b: => FreeTListOption[A]) =
      FreeTListOption(Plus[FreeT[List, Option, ?]].plus(a.f, b.f))

    def empty[A] =
      FreeTListOption(PlusEmpty[FreeT[List, Option, ?]].empty[A])

    def traverseImpl[G[_]: Applicative, A, B](fa: FreeTListOption[A])(f: A => G[B]) =
      Functor[G].map(Traverse[FreeT[List, Option, ?]].traverseImpl(fa.f)(f))(FreeTListOption.apply)

    override def foldMap[A, B: Monoid](fa: FreeTListOption[A])(f: A => B) =
      Foldable[FreeT[List, Option, ?]].foldMap(fa.f)(f)
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
    checkAll(monadPlus.laws[FreeTListOption])
    checkAll(traverse.laws[FreeTListOption])

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

    "hoistM" ! forAll { a: FreeTListOption[Int] =>
      val b = FreeTListOption(a.f.hoistM(NaturalTransformation.refl))
      Equal[FreeTListOption[Int]].equal(a, b)
    }

    "hoistN" ! forAll { a: FreeTListOption[Int] =>
      val b = FreeTListOption(a.f.hoistN(NaturalTransformation.refl))
      Equal[FreeTListOption[Int]].equal(a, b)
    }

    "hoist stack-safety" in {
      val a = (0 until 50000).foldLeft(Applicative[FreeTListOption].point(()))(
        (fu, i) => fu.flatMap(u => Applicative[FreeTListOption].point(u))
      )

      a.f.hoistM(NaturalTransformation.refl)
      a.f.hoistN(NaturalTransformation.refl)
      ()
    }

    "interpretS" ! forAll { a: FreeTListOption[Int] =>
      val b = FreeTListOption(a.f.interpretS(NaturalTransformation.refl))
      Equal[FreeTListOption[Int]].equal(a, b)
    }

    "interpretT" ! forAll { a: FreeTListOption[Int] =>
      val b = FreeTListOption(a.f.interpretT(NaturalTransformation.refl))
      Equal[FreeTListOption[Int]].equal(a, b)
    }

    "interpret stack-safety" in {
      val a = (0 until 50000).foldLeft(Applicative[FreeTListOption].point(()))(
        (fu, i) => fu.flatMap(u => Applicative[FreeTListOption].point(u))
      )

      a.f.interpretS(NaturalTransformation.refl)
      a.f.interpretT(NaturalTransformation.refl)
      ()
    }
  }

  "isoFree" ! forAll { a: FreeOption[Int] =>
    val iso = FreeT.isoFree[Option]
    Equal[FreeOption[Int]].equal(FreeOption(iso.to(iso.from(a.f))), a)
  }

  private def compilationTest = {
    val a: String \/ Int = \/-(42)
    val b: FreeT[Maybe, String \/ ?, Int] = FreeT.liftMU(a)
  }

  object instances {
    def bind[S[_]: Functor, F[_]: Functor] = Bind[FreeT[S, F, ?]]
    def foldable[S[_]: Foldable: Functor, F[_]: Foldable: Applicative: BindRec] = Foldable[FreeT[S, F, ?]]
    def traverse[S[_]: Traverse, F[_]: Traverse: Applicative: BindRec] = Traverse[FreeT[S, F, ?]]
    def monad[S[_]: Functor, F[_]: Applicative] = Monad[FreeT[S, F, ?]]
    def monadError[S[_]: Functor, F[_]: BindRec, E](implicit F: MonadError[F, E]) = MonadError[FreeT[S, F, ?], E]
    def monadState[S[_]: Functor, F[_], E](implicit F: MonadState[F, E]) = MonadState[FreeT[S, F, ?], E]
    def monadReader[S[_]: Functor, F[_], E](implicit F: MonadReader[F, E]) = MonadReader[FreeT[S, F, ?], E]
    def monadTell[S[_]: Functor, F[_], E](implicit F: MonadTell[F, E]) = MonadTell[FreeT[S, F, ?], E]
    def plus[S[_]: Functor, F[_]: Applicative: BindRec: Plus] = Plus[FreeT[S, F, ?]]
    def monadPlus[S[_]: Functor, F[_]: ApplicativePlus: BindRec] = MonadPlus[FreeT[S, F, ?]]
    def monadTrans[S[_]: Functor] = MonadTrans[FreeT[S, ?[_], ?]]

    // checking absence of ambiguity
    def functor[S[_]: Functor, F[_]: Functor] = Functor[FreeT[S, F, ?]]
    def functor[S[_]: Traverse, F[_]: Traverse: Applicative: BindRec] = Functor[FreeT[S, F, ?]]
    def foldable[S[_]: Traverse, F[_]: Traverse: Applicative: BindRec] = Foldable[FreeT[S, F, ?]]
    def bind[S[_]: Functor, F[_]: Applicative] = Bind[FreeT[S, F, ?]]
    def monad[S[_]: Functor, F[_]: ApplicativePlus: BindRec] = Monad[FreeT[S, F, ?]]
    def plus[S[_]: Functor, F[_]: ApplicativePlus: BindRec] = Plus[FreeT[S, F, ?]]
  }
}
