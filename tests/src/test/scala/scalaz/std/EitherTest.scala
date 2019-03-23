package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._

object EitherTest extends SpecLite {
  checkAll("Either", order.laws[Either[Int, Int]])
  checkAll("Either", bindRec.laws[Either[Int, ?]])
  checkAll("Either", monadError.laws[Either[Int, ?], Int])
  checkAll("Either", bifunctor.laws[Either])
  checkAll("Either", traverse.laws[Either[Int, ?]])
  checkAll("Either", bitraverse.laws[Either])
  checkAll("Either", associative.laws[Either])

  "show" in {
    import syntax.show._
    val left : Either[String, Int] = Left("leftside")
    val right : Either[String, Int] = Right(0)
    left.shows must_===("Left(\"leftside\")")
    right.shows must_===("Right(0)")
  }
}
