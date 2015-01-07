package scalaz

import org.scalacheck.Arbitrary
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalaCheckBinding._
import std.list._
import std.anyVal._
import std.option._
import org.scalacheck.Prop.forAll

object CodensityTest extends SpecLite {
  implicit def arbCodensity[F[_], A](implicit A: Arbitrary[F[A]], M: Monad[F])
      : Arbitrary[Codensity[F, A]] =
    Functor[Arbitrary].map(A)(Codensity.rep(_))

  implicit def eqlCodensity[F[_], A](implicit A: Equal[F[A]], M: Applicative[F])
      : Equal[Codensity[F, A]] =
    Equal.equalBy(_.improve)

  checkAll("List", monadPlus.laws[Codensity[List, ?]])
  checkAll("Option", monadPlus.laws[Codensity[Option, ?]])

  object instances {
    def functor[F[_]: MonadPlus, A] = Functor[Codensity[F, ?]]
    def apply[F[_]: MonadPlus, A] = Apply[Codensity[F, ?]]
    def applicative[F[_]: MonadPlus, A] = Applicative[Codensity[F, ?]]
    def plus[F[_]: MonadPlus, A] = Plus[Codensity[F, ?]]
    def monad[F[_]: MonadPlus, A] = Monad[Codensity[F, ?]]
    def monade[F[_], A] = Monad[Codensity[F, ?]]
  }
}
