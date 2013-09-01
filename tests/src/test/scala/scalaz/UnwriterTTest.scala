package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalaCheckBinding._
import std.AllInstances._
import org.scalacheck.Arbitrary

class UnwriterTTest extends Spec {

  type UnwriterTOpt[W, A] = UnwriterT[Option, W, A]
  type UnwriterTOptInt[A] = UnwriterTOpt[Int, A]

  checkAll(equal.laws[UnwriterTOptInt[Int]])
  checkAll(traverse.laws[UnwriterTOptInt])
  checkAll(bitraverse.laws[UnwriterTOpt])

  implicit def UnwriterArb[F[_], W, A](implicit W: Arbitrary[W], A: Arbitrary[A]): Arbitrary[Unwriter[W, A]] =
    Applicative[Arbitrary].apply2(W, A)(Unwriter(_, _))

  checkAll(comonad.laws[({type λ[α]=Unwriter[Int, α]})#λ])

  object instances {
    def equal[F[_], W, A](implicit E: Equal[F[(W, A)]]) = Equal[UnwriterT[F, W, A]]
    def functor[F[_]: Functor, W] = Functor[({type λ[α]=UnwriterT[F, W, α]})#λ]
    def apply[F[_]: Apply, W] = Apply[({type λ[α]=UnwriterT[F, W, α]})#λ]
    def bind[F[_]: Bind, W] = Bind[({type λ[α]=UnwriterT[F, W, α]})#λ]
    def bifunctor[F[_]: Functor] = Bifunctor[({type λ[α, β]=UnwriterT[F, α, β]})#λ]
    def bitraverse[F[_]: Traverse] = Bitraverse[({type λ[α, β]=UnwriterT[F, α, β]})#λ]
    def foldable[F[_]: Foldable, W] = Foldable[({type λ[α]=UnwriterT[F, W, α]})#λ]
    def traverse[F[_]: Traverse, W] = Traverse[({type λ[α]=UnwriterT[F, W, α]})#λ]

    object Unwriter {
      def comonad[W] = Comonad[({type λ[α]=Unwriter[W, α]})#λ]
    }
  }
}
