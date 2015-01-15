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
  checkAll(monad.laws[WriterTOptInt])
  checkAll(traverse.laws[WriterTOptInt])
  checkAll(bifunctor.laws[WriterTOpt])
  checkAll(functor.laws[NonEmptyList])
  checkAll(functor.laws[WriterT[NonEmptyList, Int, ?]])
  checkAll(bitraverse.laws[WriterTOpt])

  implicit def writerArb[F[_], W, A](implicit W: Arbitrary[W], A: Arbitrary[A]): Arbitrary[Writer[W, A]] =
    Applicative[Arbitrary].apply2(W, A)((w, a) => Writer[W, A](w, a))

  checkAll(comonad.laws[Writer[Int, ?]])

  "flatMapF consistent with flatMap" ! forAll {
    (fa: WriterTOptInt[Int], f: Int => Option[(Int, Int)]) =>
      fa.flatMapF(f) must_=== fa.flatMap(f andThen WriterT.writerT)
  }

  object instances {
    def functor[F[_]: Functor, W] = Functor[WriterT[F, W, ?]]
    def apply[F[_]: Monad, W: Semigroup] = Apply[WriterT[F, W, ?]]
    def monad[F[_]: Monad, W: Monoid] = Monad[WriterT[F, W, ?]]
    def foldable[F[_]: Foldable, W] = Foldable[WriterT[F, W, ?]]
    def traverse[F[_]: Traverse, W] = Traverse[WriterT[F, W, ?]]

    def functor[F[_]: Monad, W: Monoid] = Functor[WriterT[F, W, ?]]
    def apply[F[_]: Monad, W: Monoid] = Apply[WriterT[F, W, ?]]
    def functor[F[_]: Traverse, W: Monoid] = Functor[WriterT[F, W, ?]]
    def foldable[F[_]: Traverse, W] = Foldable[WriterT[F, W, ?]]
    
    object writer {
      def functor[W] = Functor[Writer[W, ?]]
      def apply[W: Semigroup] = Apply[Writer[W, ?]]
      def monad[W: Monoid] = Monad[Writer[W, ?]]
      def foldable[W] = Foldable[Writer[W, ?]](WriterT.writerTFoldable[Id, W])
      def traverse[W] = Traverse[Writer[W, ?]]
      def comonad[W] = Comonad[Writer[W, ?]]
    }
  }
}
