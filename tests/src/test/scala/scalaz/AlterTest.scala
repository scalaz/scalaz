package scalaz

import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalazProperties._
import scalaz.std.AllInstances._

object AlterTest extends SpecLite {

  checkAll(equal.laws[Alter[IList, Int]])

  checkAll("Alter[IList, Int]", monoid.laws[Alter[IList, Int]])
  checkAll("Alter[Maybe, Int]", monoid.laws[Alter[Maybe, Int]])

  object instances {
    def equal[F[_], A](implicit F: Equal[F[A]]) = Equal[Alter[F, A]]
    def semigroup[F[_]: Plus, A: Semigroup] = Semigroup[Alter[F, A]]
    def semigroup[F[_]: Plus, A: Monoid] = Semigroup[Alter[F, A]]
    def semigroup[F[_]: PlusEmpty, A: Semigroup] = Semigroup[Alter[F, A]]
    def semigroup[F[_]: PlusEmpty, A: Monoid] = Semigroup[Alter[F, A]]
    def monoid[F[_]: PlusEmpty, A: Monoid] = Monoid[Alter[F, A]]
  }
}
