package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalaCheckBinding._
import std.AllInstances._
import org.scalacheck.Arbitrary
import Id._
import org.scalacheck.Prop.forAll

object WriterTTest extends SpecLite {

  type WriterTOpt[W, A] = WriterT[Option, W, A]
  type WriterTOptInt[A] = WriterTOpt[Int, A]

  checkAll(equal.laws[WriterTOptInt[Int]])
  checkAll(monoid.laws[WriterTOptInt[Int]])
  checkAll(monadError.laws[({type x[E, A] = WriterT[({type y[a] = E \/ a})#y, Int, A]})#x, Int])
  checkAll(traverse.laws[WriterTOptInt])
  checkAll(monadPlus.strongLaws[WriterTOptInt])
  checkAll(bifunctor.laws[WriterTOpt])
  checkAll(functor.laws[({type λ[α]=WriterT[NonEmptyList, Int, α]})#λ])
  checkAll(bitraverse.laws[WriterTOpt])

  private[this] implicit def writerTArb0[F[_, _], E, W, A](implicit F: Arbitrary[F[E, (W, A)]]): Arbitrary[WriterT[({type l[a] = F[E, a]})#l, W, A]] =
    writerTArb[({type l[a] = F[E, a]})#l, W, A]

  private[this] implicit def writerTEqual0[F[_, _], E, W, A](implicit F: Equal[F[E, (W, A)]]): Equal[WriterT[({type l[a] = F[E, a]})#l, W, A]] =
    WriterT.writerTEqual[({type l[a] = F[E, a]})#l, W, A]

  implicit def writerArb[W, A](implicit W: Arbitrary[W], A: Arbitrary[A]): Arbitrary[Writer[W, A]] =
    Applicative[Arbitrary].apply2(W, A)((w, a) => Writer[W, A](w, a))

  checkAll(comonad.laws[({type λ[α]=Writer[Int, α]})#λ])

  "flatMapF consistent with flatMap" ! forAll {
    (fa: WriterTOptInt[Int], f: Int => Option[(Int, Int)]) =>
      fa.flatMapF(f) must_=== fa.flatMap(f andThen WriterT.writerT)
  }

  private def writerTUcompilationTest: Unit = {
    import syntax.either._
    val a: String \/ (Int, Boolean) = (1, true).right[String]
    WriterT.writerTU(a)
  }

  object instances {
    def monoid[F[_], W, A](implicit F: Monoid[F[(W,A)]]) = Monoid[WriterT[F, W, A]]
    def plus[F[_]: Plus, W] = Plus[({type λ[α]=WriterT[F, W, α]})#λ]
    def plusEmpty[F[_]: PlusEmpty, W] = PlusEmpty[({type λ[α]=WriterT[F, W, α]})#λ]
    def functor[F[_]: Functor, W] = Functor[({type λ[α]=WriterT[F, W, α]})#λ]
    def apply[F[_]: Apply, W: Semigroup] = Apply[({type λ[α]=WriterT[F, W, α]})#λ]
    def applicative[F[_]: Applicative, W: Monoid] = Applicative[({type λ[α]=WriterT[F, W, α]})#λ]
    def monad[F[_]: Monad, W: Monoid] = Monad[({type λ[α]=WriterT[F, W, α]})#λ]
    def monadPlus[F[_]: MonadPlus, W: Monoid] = MonadPlus[({type λ[α]=WriterT[F, W, α]})#λ]
    def monadError[F[_, _], W, E](implicit W: Monoid[W], F: MonadError[F, E]) = MonadError[({type x[E0, A] = WriterT[({type y[a] = F[E0, a]})#y, W, A]})#x, E]
    def foldable[F[_]: Foldable, W] = Foldable[({type λ[α]=WriterT[F, W, α]})#λ]
    def traverse[F[_]: Traverse, W] = Traverse[({type λ[α]=WriterT[F, W, α]})#λ]

    // checking absence of ambiguity
    def plus[F[_]: PlusEmpty, W] = Plus[({type λ[α]=WriterT[F, W, α]})#λ]
    def plus[F[_]: MonadPlus, W] = Plus[({type λ[α]=WriterT[F, W, α]})#λ]
    def plusEmpty[F[_]: MonadPlus, W] = PlusEmpty[({type λ[α]=WriterT[F, W, α]})#λ]
    def functor[F[_]: Apply, W: Semigroup] = Functor[({type λ[α]=WriterT[F, W, α]})#λ]
    def functor[F[_]: Apply, W: Monoid] = Functor[({type λ[α]=WriterT[F, W, α]})#λ]
    def functor[F[_]: Bind, W: Semigroup] = Functor[({type λ[α]=WriterT[F, W, α]})#λ]
    def functor[F[_]: Bind, W: Monoid] = Functor[({type λ[α]=WriterT[F, W, α]})#λ]
    def functor[F[_]: Traverse, W: Semigroup] = Functor[({type λ[α]=WriterT[F, W, α]})#λ]
    def functor[F[_]: Traverse, W: Monoid] = Functor[({type λ[α]=WriterT[F, W, α]})#λ]
    def functor[F[_]: Monad, W: Semigroup] = Functor[({type λ[α]=WriterT[F, W, α]})#λ]
    def functor[F[_]: Monad, W: Monoid] = Functor[({type λ[α]=WriterT[F, W, α]})#λ]
    def functor[F[_]: MonadPlus, W: Semigroup] = Functor[({type λ[α]=WriterT[F, W, α]})#λ]
    def functor[F[_]: MonadPlus, W: Monoid] = Functor[({type λ[α]=WriterT[F, W, α]})#λ]
    def apply[F[_]: MonadPlus, W: Monoid] = Apply[({type λ[α]=WriterT[F, W, α]})#λ]
    def apply[F[_]: Monad, W: Monoid] = Apply[({type λ[α]=WriterT[F, W, α]})#λ]
    def apply[F[_]: Monad, W: Semigroup] = Apply[({type λ[α]=WriterT[F, W, α]})#λ]
    def apply[F[_]: Bind, W: Monoid] = Apply[({type λ[α]=WriterT[F, W, α]})#λ]
    def apply[F[_]: Bind, W: Semigroup] = Apply[({type λ[α]=WriterT[F, W, α]})#λ]
    def apply[F[_]: Apply, W: Monoid] = Apply[({type λ[α]=WriterT[F, W, α]})#λ]
    def applicative[F[_]: Monad, W: Monoid] = Applicative[({type λ[α]=WriterT[F, W, α]})#λ]
    def applicative[F[_]: MonadPlus, W: Monoid] = Applicative[({type λ[α]=WriterT[F, W, α]})#λ]
    def monad[F[_]: MonadPlus, W: Monoid] = Monad[({type λ[α]=WriterT[F, W, α]})#λ]
    def foldable[F[_]: Traverse, W] = Foldable[({type λ[α]=WriterT[F, W, α]})#λ]

    object writer {
      def functor[W] = Functor[({type λ[α]=Writer[W, α]})#λ]
      def apply[W: Semigroup] = Apply[({type λ[α]=Writer[W, α]})#λ]
      def apply[W: Monoid] = Apply[({type λ[α]=Writer[W, α]})#λ]
      def applicative[W: Monoid] = Applicative[({type λ[α]=Writer[W, α]})#λ]
      def monad[W: Monoid] = Monad[({type λ[α]=Writer[W, α]})#λ]
      def foldable[W] = Foldable[({type λ[α]=Writer[W, α]})#λ](WriterT.writerTFoldable[Id, W])
      def traverse[W] = Traverse[({type λ[α]=Writer[W, α]})#λ]
      def comonad[W] = Comonad[({type λ[α]=Writer[W, α]})#λ]
    }
  }
}
