package scalaz

import scalaz.std.anyVal._
import org.scalacheck.{Arbitrary, Cogen}
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalaCheckBinding._

object DayTest extends SpecLite {

  private[this] def dayArb0[F[_], G[_], X: Cogen, Y: Cogen, A: Arbitrary](implicit
    F: Arbitrary[F[X]],
    G: Arbitrary[G[Y]]
  ): Arbitrary[Day[F, G, A]] =
    Apply[Arbitrary].apply3(
      F, G, implicitly[Arbitrary[(X, Y) => A]]
    )(Day.apply[F, G, A, X, Y](_, _, _))

  private[this] implicit def dayArb[F[_], G[_], A: Arbitrary](implicit
    F: Arbitrary[F[Int]],
    G: Arbitrary[G[Int]]
  ): Arbitrary[Day[F, G, A]] =
    dayArb0[F, G, Int, Int, A]

  private[this] implicit def cogenDay[F[_]: Applicative, A](implicit F: Cogen[F[A]]): Cogen[Day[F, F, A]] =
    F.contramap(Day.dap[F, A](_))

  private[this] implicit def dayEqual[F[_]: Applicative, A](implicit F: Equal[F[A]]): Equal[Day[F, F, A]] =
    F.contramap(Day.dap[F, A](_))

  checkAll("Day[Maybe, Maybe, ?]", applicative.laws[Day[Maybe, Maybe, ?]])
  checkAll("Day[IList, IList, ?]", applicative.laws[Day[IList, IList, ?]])

  checkAll("Day[Maybe, Maybe, ?]", cobind.laws[Day[Maybe, Maybe, ?]])
  checkAll("Day[IList, IList, ?]", cobind.laws[Day[IList, IList, ?]])

  object instances {
    def functor[F[_]: Functor, G[_]: Functor] = Functor[Day[F, G, ?]]
    def apply[F[_]: Apply, G[_]: Apply] = Apply[Day[F, G, ?]]
    def applicative[F[_]: Applicative, G[_]: Applicative] = Applicative[Day[F, G, ?]]
    def cobind[F[_]: Cobind, G[_]: Cobind] = Cobind[Day[F, G, ?]]
    def comonad[F[_]: Comonad, G[_]: Comonad] = Comonad[Day[F, G, ?]]

    def cohoist[F[_]: Comonad] = Cohoist[Lambda[(X[_], Y) => Day[F, X, Y]]]

    // checking absence of ambiguity
    def functor[F[_]: Apply      : Cobind  , G[_]: Apply      : Cobind]  = Functor[Day[F, G, ?]]
    def functor[F[_]: Apply      : Comonad , G[_]: Apply      : Comonad] = Functor[Day[F, G, ?]]
    def functor[F[_]: Applicative: Cobind  , G[_]: Applicative: Cobind]  = Functor[Day[F, G, ?]]
    def functor[F[_]: Applicative: Comonad , G[_]: Applicative: Comonad] = Functor[Day[F, G, ?]]

    def apply[F[_]: Applicative, G[_]: Applicative] = Apply[Day[F, G, ?]]
    def cobind[F[_]: Comonad, G[_]: Comonad] = Cobind[Day[F, G, ?]]
  }

}
