package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._
import org.scalacheck.Prop.forAll

object LazyEitherTest extends SpecLite {
  implicit def LazyEitherEqual[A: Equal, B: Equal]: Equal[LazyEither[A, B]] = new Equal[LazyEither[A, B]] {
    def equal(a: LazyEither[A, B], b: LazyEither[A, B]) =
      Equal[Either[A, B]].equal(a.toEither,b.toEither)
  }

  checkAll(equal.laws[LazyEither[Int,Int]])
  checkAll(monad.laws[LazyEither[Int, ?]])
  checkAll(traverse.laws[LazyEither[Int, ?]])
  checkAll(associative.laws[LazyEither])
  checkAll(bitraverse.laws[LazyEither])
  checkAll(monadError.laws[LazyEither, Int])
}
