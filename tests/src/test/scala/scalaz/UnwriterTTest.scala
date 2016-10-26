package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalaCheckBinding._
import std.AllInstances._
import org.scalacheck.{Arbitrary, Cogen}

object UnwriterTTest extends SpecLite {

  type UnwriterTOpt[W, A] = UnwriterT[Option, W, A]
  type UnwriterTOptInt[A] = UnwriterTOpt[Int, A]

  checkAll(equal.laws[UnwriterTOptInt[Int]])
  checkAll(bind.laws[UnwriterTOptInt])
  checkAll(traverse.laws[UnwriterTOptInt])
  checkAll(bitraverse.laws[UnwriterTOpt])

  implicit def UnwriterArb[F[_], W, A](implicit W: Arbitrary[W], A: Arbitrary[A]): Arbitrary[Unwriter[W, A]] =
    Applicative[Arbitrary].apply2(W, A)(Unwriter(_, _))

  private[this] implicit def unwriterCogen[W: Cogen, A: Cogen]: Cogen[Unwriter[W, A]] =
    Cogen[(W, A)].contramap(_.run)

  checkAll(comonad.laws[Unwriter[Int, ?]])

  object instances {
    def equal[F[_], W, A](implicit E: Equal[F[(W, A)]]) = Equal[UnwriterT[F, W, A]]
    def functor[F[_]: Functor, W] = Functor[UnwriterT[F, W, ?]]
    def apply[F[_]: Apply, W] = Apply[UnwriterT[F, W, ?]]
    def bind[F[_]: Bind, W] = Bind[UnwriterT[F, W, ?]]
    def bifunctor[F[_]: Functor] = Bifunctor[UnwriterT[F, ?, ?]]
    def bitraverse[F[_]: Traverse] = Bitraverse[UnwriterT[F, ?, ?]]
    def foldable[F[_]: Foldable, W] = Foldable[UnwriterT[F, W, ?]]
    def traverse[F[_]: Traverse, W] = Traverse[UnwriterT[F, W, ?]]

    object Unwriter {
      def comonad[W] = Comonad[Unwriter[W, ?]]
    }
  }
}
