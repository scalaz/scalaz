package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._
import syntax.either._

object LazyEitherTest extends SpecLite {
  implicit def LazyEitherEqual[A: Equal, B: Equal]: Equal[LazyEither[A, B]] =
    (a: LazyEither[A, B], b: LazyEither[A, B]) =>
      Equal[Either[A, B]].equal(a.toEither, b.toEither)


  checkAll(equal.laws[LazyEither[Int,Int]])
  checkAll(monad.laws[LazyEither[Int, *]])
  checkAll(alt.laws[LazyEither[Int, *]])
  checkAll(monadError.laws[LazyEither[Int, *], Int])
  checkAll(bindRec.laws[LazyEither[Int, *]])
  checkAll(traverse.laws[LazyEither[Int, *]])
  checkAll(associative.laws[LazyEither])
  checkAll(bitraverse.laws[LazyEither])

  "tail recursive tailrecM" in {
    val times = 10000

    val result =
      BindRec[LazyEither[Int, *]].tailrecM(0) {
        i => LazyEither.lazyRight(if (i < 10000) (i + 1).left[Int] else i.right[Int])
      }
    result.getOrElse(0) must_=== times
  }
}
