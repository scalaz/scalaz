package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalaCheckBinding._
import std.AllInstances._
import org.scalacheck.Arbitrary

class WriterTTest extends Spec {

  type WriterTOpt[W, A] = WriterT[Option, W, A]
  type WriterTOptInt[A] = WriterTOpt[Int, A]

  checkAll(equal.laws[WriterTOptInt[Int]])
  checkAll(monad.laws[WriterTOptInt])
  checkAll(traverse.laws[WriterTOptInt])
  checkAll(bifunctor.laws[WriterTOpt])
  checkAll(copointed.laws[({type λ[α]=WriterT[NonEmptyList, Int, α]})#λ])

  implicit def writerArb[F[_], W, A](implicit W: Arbitrary[W], A: Arbitrary[A]): Arbitrary[Writer[W, A]] =
    Applicative[Arbitrary].map2(W, A)((w, a) => Writer[W, A](w, a))

  checkAll(comonad.laws[({type λ[α]=Writer[Int, α]})#λ])

  object instances {
    def functor[F[_]: Functor, W] = Functor[({type λ[α]=WriterT[F, W, α]})#λ]
    def pointed[F[_]: Pointed, W: Monoid] = Pointed[({type λ[α]=WriterT[F, W, α]})#λ]
    def apply[F[_]: Monad, W: Semigroup] = Apply[({type λ[α]=WriterT[F, W, α]})#λ]
    def monad[F[_]: Monad, W: Monoid] = Monad[({type λ[α]=WriterT[F, W, α]})#λ]
    def foldable[F[_]: Foldable, W] = Foldable[({type λ[α]=WriterT[F, W, α]})#λ]
    def traverse[F[_]: Traverse, W] = Traverse[({type λ[α]=WriterT[F, W, α]})#λ]
    def copointed[F[_]: CoPointed, W] = CoPointed[({type λ[α]=WriterT[F, W, α]})#λ]
    def comonad[W] = CoMonad[({type λ[α]=Writer[W, α]})#λ]
  }
}
