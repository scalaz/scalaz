package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import Tags._

class EitherTest extends Spec {
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

  checkAll("Either.LeftProjection", monad.laws[({type λ[α] = Either.LeftProjection[α, Int]})#λ])
  checkAll("Either.RightProjection", monad.laws[({type λ[α] = Either.RightProjection[Int, α]})#λ])
  checkAll("Either.LeftProjection @@ First", monad.laws[({type λ[α] = Either.LeftProjection[α, Int] @@ First})#λ])
  checkAll("Either.RightProjection @@ First", monad.laws[({type λ[α] = Either.RightProjection[Int, α] @@ First})#λ])
  checkAll("Either.LeftProjection @@ Last", monad.laws[({type λ[α] = Either.LeftProjection[α, Int] @@ Last})#λ])
  checkAll("Either.RightProjection @@ Last", monad.laws[({type λ[α] = Either.RightProjection[Int, α] @@ Last})#λ])

  checkAll("Either", monad.laws[({type f[x] = Either[Int, x]})#f])
}
