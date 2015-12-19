package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import Tags._

object EitherTest extends SpecLite {
  checkAll("Either", order.laws[Either[Int, Int]])
  checkAll("Either.LeftProjection", order.laws[Either.LeftProjection[Int, Int]])
  checkAll("Either.LeftProjection @@ First", order.laws[Either.LeftProjection[Int, Int] @@ First])
  checkAll("Either.LeftProjection @@ Last", order.laws[Either.LeftProjection[Int, Int] @@ Last])
  checkAll("Either.RightProjection", order.laws[Either.RightProjection[Int, Int]])
  checkAll("Either.RightProjection @@ First", order.laws[Either.RightProjection[Int, Int] @@ First])
  checkAll("Either.RightProjection @@ Last", order.laws[Either.RightProjection[Int, Int] @@ Last])

  checkAll("Either.LeftProjection", monoid.laws[Either.LeftProjection[Int, Int]])
  checkAll("Either.LeftProjection @@ First", monoid.laws[Either.LeftProjection[Int, Int] @@ First])
  checkAll("Either.LeftProjection @@ Last", monoid.laws[Either.LeftProjection[Int, Int] @@ Last])
  checkAll("Either.RightProjection", monoid.laws[Either.RightProjection[Int, Int]])
  checkAll("Either.RightProjection @@ First", monoid.laws[Either.RightProjection[Int, Int] @@ First])
  checkAll("Either.RightProjection @@ Last", monoid.laws[Either.RightProjection[Int, Int] @@ Last])

  checkAll("Either.LeftProjection", monad.laws[Either.LeftProjection[?, Int]])
  checkAll("Either.RightProjection", monad.laws[Either.RightProjection[Int, ?]])
  checkAll("Either.LeftProjection @@ First", monad.laws[λ[α => Either.LeftProjection[α, Int] @@ First]])
  checkAll("Either.RightProjection @@ First", monad.laws[λ[α => Either.RightProjection[Int, α] @@ First]])
  checkAll("Either.LeftProjection @@ Last", monad.laws[λ[α => Either.LeftProjection[α, Int] @@ Last]])
  checkAll("Either.RightProjection @@ Last", monad.laws[λ[α => Either.RightProjection[Int, α] @@ Last]])

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
