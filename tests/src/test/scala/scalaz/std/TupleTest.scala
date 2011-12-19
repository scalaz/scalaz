package scalaz
package std

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

class TupleTest extends Spec {

  type A = Int
  type B = Int
  type C = Int
  type D = Int
  type E = Int
  type F = Int
  type G = Int
  type H = Int
  type K = Int
  type V = Int
  type X = Int

  checkAll("Tuple1", order.laws[(A)])
  checkAll("Tuple2", order.laws[(A, B)])
  checkAll("Tuple3", order.laws[(A, B, C)])
  checkAll("Tuple4", order.laws[(A, B, C, D)])
  checkAll("Tuple5", order.laws[(A, B, C, D, E)])
  checkAll("Tuple6", order.laws[(A, B, C, D, E, F)])
  checkAll("Tuple7", order.laws[(A, B, C, D, E, F, G)])
  checkAll("Tuple8", order.laws[(A, B, C, D, E, F, G, H)])

  checkAll("Tuple1", group.laws[(A)])
  checkAll("Tuple2", group.laws[(A, B)])
  checkAll("Tuple3", group.laws[(A, B, C)])
  checkAll("Tuple4", group.laws[(A, B, C, D)])
  checkAll("Tuple5", group.laws[(A, B, C, D, E)])
  checkAll("Tuple6", group.laws[(A, B, C, D, E, F)])
  checkAll("Tuple7", group.laws[(A, B, C, D, E, F, G)])
  checkAll("Tuple8", group.laws[(A, B, C, D, E, F, G, H)])

  checkAll("Tuple1", monad.laws[Tuple1])
  checkAll("Tuple2", monad.laws[({type λ[α] = (B, α)})#λ])
  checkAll("Tuple3", monad.laws[({type λ[α] = (B, C, α)})#λ])
  checkAll("Tuple4", monad.laws[({type λ[α] = (B, C, D, α)})#λ])
  checkAll("Tuple5", monad.laws[({type λ[α] = (B, C, D, E, α)})#λ])
  checkAll("Tuple6", monad.laws[({type λ[α] = (B, C, D, E, F, α)})#λ])
  checkAll("Tuple7", monad.laws[({type λ[α] = (B, C, D, E, F, G, α)})#λ])
  checkAll("Tuple8", monad.laws[({type λ[α] = (B, C, D, E, F, G, H, α)})#λ])
}
