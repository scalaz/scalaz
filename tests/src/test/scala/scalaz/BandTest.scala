package scalaz

import scalaz.std.AllInstances._
import scalaz.scalacheck.ScalazProperties.band
import scalaz.scalacheck.ScalaCheckBinding._
import org.scalacheck.Arbitrary
import Tags._

object BandTest extends SpecLite {

  implicit def tagArb[A, B](implicit A: Arbitrary[A]): Arbitrary[A @@ B] =
    Functor[Arbitrary].map(A)(Tag.apply[A, B])

  implicit def tagEqual[A, B](implicit A: Equal[A]): Equal[A @@ B] =
    A.contramap(Tag.unwrap[A, B])

  checkAll(band.laws[Int @@ FirstVal])
  checkAll(band.laws[Int @@ LastVal])

}
