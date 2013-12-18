package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalaCheckBinding._
import org.scalacheck.Arbitrary

object EndoTest extends SpecLite {

  implicit def endoArb[A](implicit A: Arbitrary[A => A]): Arbitrary[Endo[A]] =
    Functor[Arbitrary].map(A)(Endo.endo)

  implicit val endoIntEqual: Equal[Endo[Int]] =
    Equal.equal( (a, b) =>
      Iterator.fill(20)(util.Random.nextInt).forall(n => a(n) == b(n))
    )

  checkAll(invariantFunctor.laws[Endo])

}
