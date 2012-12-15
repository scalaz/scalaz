package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._

class LazyEitherTest extends Spec {
  implicit def LazyEitherEqual[A: Equal, B: Equal]: Equal[LazyEither[A, B]] = new Equal[LazyEither[A, B]] {
    def equal(a: LazyEither[A, B], b: LazyEither[A, B]) =
      Equal[Either[A, B]].equal(a.toEither,b.toEither)
  }

  checkAll(equal.laws[LazyEither[Int,Int]])
  checkAll(monad.laws[({type f[a]=LazyEither[Int,a]})#f])
  checkAll(traverse.laws[({type f[a]=LazyEither[Int,a]})#f])
  checkAll(bitraverse.laws[LazyEither])
}
