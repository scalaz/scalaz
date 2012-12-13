package scalaz

import std.AllInstances._
import scalaz.testlib.ScalazProperties._
import scalaz.testlib.ScalazArbitrary._

class LazyTupleTest extends testlib.Spec {

  type A = Int
  type B = Int
  type C = Int
  type D = Int

  checkAll("LazyTuple2", order.laws[LazyTuple2[A, B]])
  checkAll("LazyTuple3", order.laws[LazyTuple3[A, B, C]])
  checkAll("LazyTuple4", order.laws[LazyTuple4[A, B, C, D]])

  checkAll("LazyTuple2", group.laws[LazyTuple2[A, B]])
  checkAll("LazyTuple3", group.laws[LazyTuple3[A, B, C]])
  checkAll("LazyTuple4", group.laws[LazyTuple4[A, B, C, D]])

  checkAll("LazyTuple2", monad.laws[({type λ[α] = LazyTuple2[B, α]})#λ])
  checkAll("LazyTuple3", monad.laws[({type λ[α] = LazyTuple3[B, C, α]})#λ])
  checkAll("LazyTuple4", monad.laws[({type λ[α] = LazyTuple4[B, C, D, α]})#λ])
}
