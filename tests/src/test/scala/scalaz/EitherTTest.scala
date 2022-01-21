package scalaz

import java.util.concurrent.atomic.AtomicInteger

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._
import org.scalacheck.Prop.forAll
import Tags.Parallel

object EitherTTest extends SpecLite {

  type ValidationInt[A] = Validation[Int, A]
  type EitherTList[A, B] = EitherT[List, A, B]
  type EitherTListInt[A] = EitherT[List, Int, A]
  type EitherTOptionInt[A] = EitherT[Option, Int, A]
  type EitherTValidationInt[A] = EitherT[ValidationInt, Int, A] @@ Parallel
  type EitherTComputation[A] = EitherT[Function0, Int, A] // in lieu of IO

  implicit val validationIntParalellelApplicative: Applicative.Par[ValidationInt] =
    Parallel.subst1[Applicative, ValidationInt](Applicative[ValidationInt])

  implicit def equalParallel[A: Equal]: Equal[A @@ Tags.Parallel] =
    Tags.Parallel.subst(Equal[A])

  checkAll(equal.laws[EitherTListInt[Int]])
  checkAll(bindRec.laws[EitherTListInt])
  checkAll(monadPlus.laws[EitherTListInt])
  checkAll(monadError.laws[EitherTListInt, Int])
  checkAll(traverse.laws[EitherTListInt])
  checkAll(bitraverse.laws[EitherTList])
  checkAll(applicative.laws[EitherTValidationInt])

  "consistent Bifoldable" ! forAll { (a: EitherTList[Int, Int]) =>
    val F = new Bitraverse[EitherTList]{
      def bitraverseImpl[G[_]: Applicative, A, B, C, D](fab: EitherTList[A, B])(f: A => G[C], g: B => G[D]) =
        EitherT.eitherTBitraverse[List].bitraverseImpl(fab)(f, g)
    }

    Bifoldable[EitherTList].bifoldMap(a)(_ :: Nil)(_ :: Nil) must_=== F.bifoldMap(a)(_ :: Nil)(_ :: Nil)
  }

  "show" ! forAll { (a: EitherTList[Int, Int]) =>
    Show[EitherTList[Int, Int]].show(a) must_=== Show[List[Int \/ Int]].show(a.run)
  }

  "fromDisjunction" ! forAll { (a: String \/ Int) =>
    Option(a.isLeft) must_=== EitherT.fromDisjunction[Option](a).isLeft
  }

  "flatMapF consistent with flatMap" ! forAll { (a: EitherTList[Int, Int], f: Int => List[Int \/ String]) =>
    a.flatMap(f andThen EitherT.apply) must_=== a.flatMapF(f)
  }

  "orElse only executes the left hand monad once" should {
    val counter = new AtomicInteger(0)
    val inc: EitherTComputation[Int] = EitherT.rightT(() => counter.incrementAndGet())
    val other: EitherTComputation[Int] = EitherT.rightT(() => 0) // does nothing

    (inc orElse other).run.apply() must_== \/-(1)
    counter.get() must_== 1
  }

  object instances {
    def functor[F[_] : Functor, A] = Functor[EitherT[F, A, *]]
    def bindRec[F[_] : Monad: BindRec, A] = BindRec[EitherT[F, A, *]]
    def monad[F[_] : Monad, A] = Monad[EitherT[F, A, *]]
    def plus[F[_] : Monad, A: Semigroup] = Plus[EitherT[F, A, *]]
    def monadPlus[F[_] : Monad, A: Monoid] = MonadPlus[EitherT[F, A, *]]
    def foldable[F[_] : Foldable, A] = Foldable[EitherT[F, A, *]]
    def traverse[F[_] : Traverse, A] = Traverse[EitherT[F, A, *]]
    def bifunctor[F[_] : Functor] = Bifunctor[EitherT[F, *, *]]
    def bifoldable[F[_] : Foldable] = Bifoldable[EitherT[F, *, *]]
    def bitraverse[F[_] : Traverse] = Bitraverse[EitherT[F, *, *]]
    def parallel[F[_] : Applicative.Par, E] = implicitly[Applicative.Par[EitherT[F, E, *]]]

    // checking absence of ambiguity
    def functor[A, F[_] : BindRec] = Functor[EitherT[F, A, *]]
    def functor[A, F[_] : Monad] = Functor[EitherT[F, A, *]]
    def functor[A, F[_] : Monad : BindRec] = Functor[EitherT[F, A, *]]
    def functor[A, F[_] : Traverse] = Functor[EitherT[F, A, *]]
    def functor[A, F[_] : Monad: Traverse] = Functor[EitherT[F, A, *]]
    def functor[A, F[_] : BindRec: Traverse] = Functor[EitherT[F, A, *]]
    def functor[A, F[_] : Monad: BindRec: Traverse] = Functor[EitherT[F, A, *]]
    def functor[A: Monoid, F[_] : Monad] = Functor[EitherT[F, A, *]]
    def functor[A: Monoid, F[_] : Monad : BindRec] = Functor[EitherT[F, A, *]]
    def functor[A: Monoid, F[_] : Monad : Traverse] = Functor[EitherT[F, A, *]]
    def apply[F[_] : Monad, A] = Apply[EitherT[F, A, *]]
    def apply[F[_] : Monad, A: Monoid] = Apply[EitherT[F, A, *]]
    def apply[F[_] : Monad : BindRec, A] = Apply[EitherT[F, A, *]]
    def apply[F[_] : Monad : BindRec, A: Monoid] = Apply[EitherT[F, A, *]]
    def monad[F[_] : Monad, A: Monoid] = Monad[EitherT[F, A, *]]
    def plus[F[_] : Monad, A: Monoid] = Plus[EitherT[F, A, *]]
    def foldable[F[_] : Traverse, A] = Foldable[EitherT[F, A, *]]
    def bifunctor[F[_] : Traverse] = Bifunctor[EitherT[F, *, *]]
    def bifoldable[F[_] : Traverse] = Bifoldable[EitherT[F, *, *]]
    def monadError[F[_] : Monad, A] = MonadError[EitherT[F, A, *], A]
  }

}
