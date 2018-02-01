package scalaz

import scalaz.scalacheck.ScalazArbitrary._
import scalaz.scalacheck.ScalazProperties._
import scalaz.std.AllInstances._

object ApTest extends SpecLite {

  checkAll(equal.laws[Ap[IList, Int]])

  checkAll("Ap[IList, Int]", monoid.laws[Ap[IList, Int]])
  checkAll("Ap[Maybe, Int]", monoid.laws[Ap[Maybe, Int]])

  object instances {
    def semigroup[F[_]: Apply, A: Semigroup] = Semigroup[Ap[F, A]]
    def semigroup[F[_]: Applicative, A: Semigroup] = Semigroup[Ap[F, A]]
    def semigroup[F[_]: Apply, A: Monoid] = Semigroup[Ap[F, A]]
    def semigroup[F[_]: Applicative, A: Monoid] = Semigroup[Ap[F, A]]
    def monoid[F[_]: Applicative, A: Monoid] = Monoid[Ap[F, A]]
  }
}
