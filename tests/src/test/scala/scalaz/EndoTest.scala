package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

object EndoTest extends SpecLite {

  implicit val endoIntEqual: Equal[Endo[Int]] =
    Equal.equal( (a, b) =>
      Iterator.fill(20)(util.Random.nextInt).forall(n => a(n) == b(n))
    )

  checkAll(invariantFunctor.laws[Endo])

}
