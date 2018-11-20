package scalaz

import Scalaz._
import org.scalacheck.Arbitrary
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalaCheckBinding._

import scala.language.higherKinds

class DensityTest extends SpecLite {

  implicit def densityArb[F[_], A](implicit A: Arbitrary[F[A]], W: Comonad[F]): Arbitrary[Density[F, A]] =
    Functor[Arbitrary].map(A)(Density.liftDensity(_))

  implicit def nelDensityArb[F[_], A](implicit AR: Arbitrary[A], W: Comonad[F]): Arbitrary[Density[F,A] => A] = {
    def ff(a: A): Density[F,A] => A = _ => a
    Functor[Arbitrary].map(AR)(ff)
  }

  implicit def endoIntEqual[T,R[_]]: Equal[Density[R, T]] = Equal.equal( (a, b) => a.runDensity == b.runDensity)

  type PairInt[A] = (Int, A)

  checkAll("Density[NonEmptyList, ?]", comonad.laws[Density[NonEmptyList, ?]])
  checkAll("Density[PairInt, ?]", comonad.laws[Density[PairInt, ?]])

  object instances {
    def forFree[F[_]] = Comonad[Density[F, ?]]
    def comonad[F[_]: Comonad] = Comonad[Density[F, ?]]
    def combind[F[_]: Cobind] = Comonad[Density[F, ?]]
  }
}
