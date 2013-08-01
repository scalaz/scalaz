package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import std.AllInstances._

class LazyEitherTTest extends Spec {

  // TODO duplication with LazyEitherTest
  implicit def LazyEitherEqual[A: Equal, B: Equal]: Equal[LazyEither[A, B]] = new Equal[LazyEither[A, B]] {
    def equal(a: LazyEither[A, B], b: LazyEither[A, B]) =
      Equal[Either[A, B]].equal(a.toEither,b.toEither)
  }

  implicit def lazyEitherTEqual[F[_], A, B](implicit F0: Equal[F[LazyEither[A, B]]]): Equal[LazyEitherT[F, A, B]] =
    F0.contramap((_: LazyEitherT[F, A, B]).run)

  type LazyEitherTList[A, B] = LazyEitherT[List, A, B]
  type LazyEitherTListInt[A] = LazyEitherT[List, Int, A]

  checkAll(equal.laws[LazyEitherTListInt[Int]])
  checkAll(monad.laws[LazyEitherTListInt])
  checkAll(traverse.laws[LazyEitherTListInt])
  checkAll(bifunctor.laws[LazyEitherTList])

}
