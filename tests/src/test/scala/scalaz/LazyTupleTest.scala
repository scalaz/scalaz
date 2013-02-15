package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class LazyTupleTest extends Spec {

  type A = Int
  type B = Int
  type C = Int
  type D = Int

  checkAll("LazyTuple2", bitraverse.laws[LazyTuple2])

  checkAll("LazyTuple2", order.laws[LazyTuple2[A, B]])
  checkAll("LazyTuple3", order.laws[LazyTuple3[A, B, C]])
  checkAll("LazyTuple4", order.laws[LazyTuple4[A, B, C, D]])

  checkAll("LazyTuple2", monoid.laws[LazyTuple2[A, B]])
  checkAll("LazyTuple3", monoid.laws[LazyTuple3[A, B, C]])
  checkAll("LazyTuple4", monoid.laws[LazyTuple4[A, B, C, D]])

  checkAll("LazyTuple2", monad.laws[({type λ[α] = LazyTuple2[B, α]})#λ])
  checkAll("LazyTuple3", monad.laws[({type λ[α] = LazyTuple3[B, C, α]})#λ])
  checkAll("LazyTuple4", monad.laws[({type λ[α] = LazyTuple4[B, C, D, α]})#λ])
}
