package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalaCheckBinding._
import std.AllInstances._
import org.scalacheck.{Arbitrary, Cogen}
import Id._
import org.scalacheck.Prop.forAll

object WriterTTest extends SpecLite {

  type WriterTOpt[W, A] = WriterT[W, Option, A]
  type WriterTOptInt[A] = WriterTOpt[Int, A]
  type IntOr[A] = Int \/ A
  type WriterTEither[A] = WriterT[Int, IntOr, A]
  type ConstInt[A] = Const[Int, A]

  checkAll(equal.laws[WriterTOptInt[Int]])
  checkAll(monoid.laws[WriterTOptInt[Int]])
  checkAll(monadError.laws[WriterTEither, Int])
  checkAll(traverse.laws[WriterTOptInt])
  checkAll(bindRec.laws[WriterTOptInt])
  checkAll(monadPlus.strongLaws[WriterTOptInt])
  checkAll(bifunctor.laws[WriterTOpt])
  checkAll(functor.laws[WriterT[Int, NonEmptyList, *]])
  checkAll(bitraverse.laws[WriterTOpt])
  checkAll(monadTrans.laws[({type l[a[_], b] = WriterT[Int, a, b]})#l, List])
  checkAll(divisible.laws[WriterT[Int, ConstInt, *]])

  implicit def writerArb[W, A](implicit W: Arbitrary[W], A: Arbitrary[A]): Arbitrary[Writer[W, A]] =
    Applicative[Arbitrary].apply2(W, A)((w, a) => Writer[W, A](w, a))

  private[this] implicit def writerCogen[W: Cogen, A: Cogen]: Cogen[Writer[W, A]] =
    Cogen[(W, A)].contramap(_.run)

  checkAll(comonad.laws[Writer[Int, *]])

  "flatMapF consistent with flatMap" ! forAll {
    (fa: WriterTOptInt[Int], f: Int => Option[(Int, Int)]) =>
      fa.flatMapF(f) must_=== fa.flatMap(f andThen WriterT.writerT)
  }

  "mapF consistent with map" ! forAll {
    (fa: WriterTOptInt[Int], f: Int => String) =>
      fa.mapF(f andThen (s => Applicative[Option].point(s))) must_=== fa.map(f)
  }

  private def writerTUcompilationTest: Unit = {
    import syntax.either._
    val a: String \/ (Int, Boolean) = (1, true).right[String]
    WriterT.writerTU(a)
  }

  object instances {
    def monoid[W, F[_], A](implicit F: Monoid[F[(W,A)]]) = Monoid[WriterT[W, F, A]]
    def plus[W, F[_]: Plus] = Plus[WriterT[W, F, *]]
    def plusEmpty[W, F[_]: PlusEmpty] = PlusEmpty[WriterT[W, F, *]]
    def functor[W, F[_]: Functor] = Functor[WriterT[W, F, *]]
    def apply[W: Semigroup, F[_]: Apply] = Apply[WriterT[W, F, *]]
    def applicative[W: Monoid, F[_]: Applicative] = Applicative[WriterT[W, F, *]]
    def bind[W: Semigroup, F[_]: Bind] = Bind[WriterT[W, F, *]]
    def bindRec[W: Semigroup, F[_]: BindRec: Applicative] = BindRec[WriterT[W, F, *]]
    def monad[W: Monoid, F[_]: Monad] = Monad[WriterT[W, F, *]]
    def monadPlus[W: Monoid, F[_]: MonadPlus] = MonadPlus[WriterT[W, F, *]]
    def monadError[W: Monoid, F[_], E](implicit F: MonadError[F, E]) = MonadError[WriterT[W, F, *], E]
    def foldable[W, F[_]: Foldable] = Foldable[WriterT[W, F, *]]
    def traverse[W, F[_]: Traverse] = Traverse[WriterT[W, F, *]]
    def decidable[W, F[_] : Decidable] = Decidable[WriterT[W, F, *]]
    def divisible[W, F[_] : Divisible] = Divisible[WriterT[W, F, *]]

    // checking absence of ambiguity
    def plus[W, F[_]: PlusEmpty] = Plus[WriterT[W, F, *]]
    def plus[W, F[_]: MonadPlus] = Plus[WriterT[W, F, *]]
    def plusEmpty[W, F[_]: MonadPlus] = PlusEmpty[WriterT[W, F, *]]
    def functor[W: Semigroup, F[_]: Apply] = Functor[WriterT[W, F, *]]
    def functor[W: Monoid, F[_]: Apply] = Functor[WriterT[W, F, *]]
    def functor[W: Semigroup, F[_]: Bind] = Functor[WriterT[W, F, *]]
    def functor[W: Monoid, F[_]: Bind] = Functor[WriterT[W, F, *]]
    def functor[W: Semigroup, F[_]: Traverse] = Functor[WriterT[W, F, *]]
    def functor[W: Monoid, F[_]: Traverse] = Functor[WriterT[W, F, *]]
    def functor[W: Semigroup, F[_]: Monad] = Functor[WriterT[W, F, *]]
    def functor[W: Monoid, F[_]: Monad] = Functor[WriterT[W, F, *]]
    def functor[W: Semigroup, F[_]: MonadPlus] = Functor[WriterT[W, F, *]]
    def functor[W: Monoid, F[_]: MonadPlus] = Functor[WriterT[W, F, *]]
    def apply[W: Monoid, F[_]: MonadPlus] = Apply[WriterT[W, F, *]]
    def apply[W: Monoid, F[_]: Monad] = Apply[WriterT[W, F, *]]
    def apply[W: Semigroup, F[_]: Monad] = Apply[WriterT[W, F, *]]
    def apply[W: Monoid, F[_]: Bind] = Apply[WriterT[W, F, *]]
    def apply[W: Semigroup, F[_]: Bind] = Apply[WriterT[W, F, *]]
    def apply[W: Monoid, F[_]: Apply] = Apply[WriterT[W, F, *]]
    def applicative[W: Monoid, F[_]: Monad] = Applicative[WriterT[W, F, *]]
    def applicative[W: Monoid, F[_]: MonadPlus] = Applicative[WriterT[W, F, *]]
    def bind[W: Monoid, F[_]: MonadPlus] = Bind[WriterT[W, F, *]]
    def bind[W: Monoid, F[_]: Monad] = Bind[WriterT[W, F, *]]
    def bind[W: Semigroup, F[_]: Monad] = Bind[WriterT[W, F, *]]
    def bind[W: Monoid, F[_]: Bind] = Bind[WriterT[W, F, *]]
    def bindRec[W: Semigroup, F[_]: BindRec: Monad] = BindRec[WriterT[W, F, *]]
    def bindRec[W: Monoid, F[_]: BindRec: Applicative] = BindRec[WriterT[W, F, *]]
    def bindRec[W: Monoid, F[_]: BindRec: Monad] = BindRec[WriterT[W, F, *]]
    def bindRec[W: Semigroup, F[_]: BindRec: MonadPlus] = BindRec[WriterT[W, F, *]]
    def bindRec[W: Monoid, F[_]: BindRec: MonadPlus] = BindRec[WriterT[W, F, *]]
    def monad[W: Monoid, F[_]: MonadPlus] = Monad[WriterT[W, F, *]]
    def foldable[W, F[_]: Traverse] = Foldable[WriterT[W, F, *]]
    def divisible[W, F[_] : Decidable] = Divisible[WriterT[W, F, *]]

    object writer {
      def functor[W] = Functor[Writer[W, *]]
      def apply[W: Semigroup] = Apply[Writer[W, *]]
      def apply[W: Monoid] = Apply[Writer[W, *]]
      def applicative[W: Monoid] = Applicative[Writer[W, *]]
      def bind[W: Semigroup] = Bind[Writer[W, *]]
      def bind[W: Monoid] = Bind[Writer[W, *]]
      def monad[W: Monoid] = Monad[Writer[W, *]]
      def foldable[W] = Foldable[Writer[W, *]](WriterT.writerTFoldable[W, Id])
      def traverse[W] = Traverse[Writer[W, *]]
      def comonad[W] = Comonad[Writer[W, *]]
    }
  }
}
