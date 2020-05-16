package scalaz

import org.scalacheck.{Arbitrary, Gen}
import org.scalacheck.Prop.forAll
import std.AllInstances._
import FreeT._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalaCheckBinding._
import scalaz.syntax.monad._

object FreeTTest extends SpecLite {
  type FreeTList[M[_], A] = FreeT[List, M, A]
  type FreeTListOption[A] = FreeTList[Option, A]

  implicit def freeTListOptionArb[A](implicit A: Arbitrary[A]): Arbitrary[FreeTListOption[A]] =
    Arbitrary(FreeTTest.freeTGen[List, Option, A](
      Gen.choose(0, 2).flatMap(Gen.listOfN(_, freeTListOptionArb[A].arbitrary))
    ))

  implicit def freeTListOptionEq[A](implicit A: Equal[A]): Equal[FreeTListOption[A]] = new Equal[FreeTListOption[A]] {
    def equal(a: FreeTListOption[A], b: FreeTListOption[A]) = Equal[Option[A]].equal(a.runM(_.headOption), b.runM(_.headOption))
  }

  def freeTGen[F[_], G[_], A](g: Gen[F[FreeT[F, G, A]]])(implicit G: Applicative[G], A: Arbitrary[A]): Gen[FreeT[F, G, A]] =
    Gen.frequency(
      (1, Functor[Arbitrary].map(A)(FreeT.point[F, G, A](_)).arbitrary),
      (1, Functor[Arbitrary].map(Arbitrary(g))(FreeT.liftF[F, G, FreeT[F, G, A]](_).flatMap(x => x)).arbitrary)
    )

  object headOption extends (List ~> Option) {
    def apply[A](l: List[A]): Option[A] = l.headOption
  }

  "ListOption" should {
    checkAll(monadPlus.laws[FreeTListOption])
    checkAll(alt.laws[FreeTListOption])
    checkAll(traverse.laws[FreeTListOption])
    checkAll(monadTrans.laws[FreeTList, Option])

    "lawful MonadPlus" in {
      // give names to some expressions
      val f: Unit => FreeTListOption[Unit] = _ => FreeT.liftM(MonadPlus[Option].empty)
      val a = ()
      val g = ().point[FreeTListOption]

      // by the monad laws, f1 = f2
      val f1 = a.point[FreeTListOption] flatMap f
      val f2 = f(a)

      // by the substitution property of equality,
      // when f1 = f2, then also fg1 = fg2
      val fg1 = MonadPlus[FreeTListOption].plus(f1, g)
      val fg2 = MonadPlus[FreeTListOption].plus(f2, g)

      // so let's check that
      Equal[FreeTListOption[Unit]].equal(fg1, fg2)
    }

    "not stack overflow with 50k binds" in {
      val expected = Applicative[FreeTListOption].point(())
      val result =
        BindRec[FreeTListOption].tailrecM(0)(i =>
          if (i < 50000)
            Applicative[FreeTListOption].point(\/.left[Int, Unit](i + 1))
          else
            Applicative[FreeTListOption].point(\/.right[Int, Unit](()))
        )

      Equal[FreeTListOption[Unit]].equal(expected, result)
    }

    "not stack overflow with 50k left-associated binds" in {
      val expected = Applicative[FreeTListOption].point(())
      val result =
        (0 until 50000).foldLeft(Applicative[FreeTListOption].point(()))(
          (fu, i) => fu.flatMap(u => Applicative[FreeTListOption].point(u))
        )

      Equal[FreeTListOption[Unit]].equal(expected, result)
    }

    "not stack overflow with bind followed by 50k maps" in {
      val expected = Applicative[FreeTListOption].point(())
      val result =
        (0 until 50000).foldLeft(().point[FreeTListOption].flatMap(u => u.point[FreeTListOption]))(
          (fu, i) => fu.map(u => u)
        )

      Equal[FreeTListOption[Unit]].equal(expected, result)
    }

    "hoist" ! forAll { (a: FreeTListOption[Int]) =>
      val b = a.hoist(NaturalTransformation.refl)
      Equal[FreeTListOption[Int]].equal(a, b)
    }

    "hoist stack-safety" in {
      val a = (0 until 50000).foldLeft(Applicative[FreeTListOption].point(()))(
        (fu, i) => fu.flatMap(u => Applicative[FreeTListOption].point(u))
      )

      val b = a.hoist(NaturalTransformation.refl) // used to overflow
    }

    "interpret" ! forAll { (a: FreeTListOption[Int]) =>
      val b = a.interpret(NaturalTransformation.refl)
      Equal[FreeTListOption[Int]].equal(a, b)
    }

    "interpret stack-safety" in {
      val a = (0 until 50000).foldLeft(Applicative[FreeTListOption].point(()))(
        (fu, i) => fu.flatMap(u => Applicative[FreeTListOption].point(u))
      )

      val b = a.interpret(NaturalTransformation.refl) // used to overflow
    }

    "foldMap should be consistent with runM" ! forAll { (a: FreeTListOption[Int]) =>
      val x = a.runM(_.headOption)
      val y = a.foldMap(headOption)
      Equal[Option[Int]].equal(x, y)
    }
  }

  "#1156: equals should not return true for obviously unequal instances" in {
    val a = FreeT.point[List, Option, Int](1).flatMap(x => FreeT.point(2))
    val b = FreeT.point[List, Option, Int](3).flatMap(x => FreeT.point(4))
    a != b
  }

  "isoFree" ! forAll { (a: FreeOption[Int]) =>
    val iso = FreeT.isoFree[Option]
    Equal[FreeOption[Int]].equal(FreeOption(iso.to(iso.from(a.f))), a)
  }

  private def compilationTest = {
    val a: String \/ Int = \/-(42)
    val b: FreeT[Maybe, \/[String, *], Int] = FreeT.liftMU[Maybe, String \/ Int](a)
  }

  object instances {
    def bind[S[_]: Functor, F[_]: Applicative] = Bind[FreeT[S, F, *]]
    def foldable[S[_]: Foldable: Functor, F[_]: Foldable: Applicative: BindRec] = Foldable[FreeT[S, F, *]]
    def traverse[S[_]: Traverse, F[_]: Traverse: Applicative: BindRec] = Traverse[FreeT[S, F, *]]
    def monad[S[_]: Functor, F[_]: Applicative] = Monad[FreeT[S, F, *]]
    def monadError[S[_]: Functor, F[_]: BindRec, E](implicit F: MonadError[F, E]) = MonadError[FreeT[S, F, *], E]
    def monadState[S[_]: Functor, F[_], E](implicit F: MonadState[F, E]) = MonadState[FreeT[S, F, *], E]
    def monadReader[S[_]: Functor, F[_], E](implicit F: MonadReader[F, E]) = MonadReader[FreeT[S, F, *], E]
    def monadTell[S[_]: Functor, F[_], E](implicit F: MonadTell[F, E]) = MonadTell[FreeT[S, F, *], E]
    def plus[S[_]: Functor, F[_]: Applicative: BindRec: Plus] = Plus[FreeT[S, F, *]]
    def monadPlus[S[_]: Functor, F[_]: ApplicativePlus: BindRec] = MonadPlus[FreeT[S, F, *]]
    def alt[S[_]: Functor, F[_]: ApplicativePlus: BindRec] = Alt[FreeT[S, F, *]]
    def monadTrans[S[_]: Functor] = MonadTrans[({type l[a[_], b] = FreeT[S, a, b]})#l]

    // checking absence of ambiguity
    def functor[S[_]: Traverse, F[_]: Traverse: Applicative: BindRec] = Functor[FreeT[S, F, *]]
    def foldable[S[_]: Traverse, F[_]: Traverse: Applicative: BindRec] = Foldable[FreeT[S, F, *]]
    def monad[S[_]: Functor, F[_]: ApplicativePlus: BindRec] = Monad[FreeT[S, F, *]]
    def plus[S[_]: Functor, F[_]: ApplicativePlus: BindRec] = Plus[FreeT[S, F, *]]

    object issue_1308 {
      type G[A] = State[Byte, A]
      type F[A] = FreeT[Id.Id, G, A]
      MonadState[F, Byte]
    }
  }
}
