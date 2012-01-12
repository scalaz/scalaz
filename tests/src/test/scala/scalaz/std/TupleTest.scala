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

  checkAll("Tuple1", comonad.laws[Tuple1])
  checkAll("Tuple2", comonad.laws[({type λ[α]=(Int, α)})#λ])

  "syntax" should {
    import std.tuple.tupleSyntax._
    "mapElements" in {
       (1, 2).mapElements(_1 = 2 *) must be_===((2, 2))
    }
    "toIndexedSeq" in {
      val as: IndexedSeq[Int] = (1, 2).toIndexedSeq
      as.toList must be_===(List(1, 2))
    }
    "fold" in {
      (1, 2).fold(_ + _) must be_===(3)
    }
  }

  object instances {
    object tuple1 {
      def show[A: Show] = Show[Tuple1[A]]
      def equal[A: Equal] = Equal[Tuple1[A]]
      def order[A: Order] = Order[Tuple1[A]]
      def semigroup[A: Semigroup] = Semigroup[Tuple1[A]]
      def monoid[A: Monoid] = Monoid[Tuple1[A]]
      
      def monad = Monad[Tuple1]
      def comonad = CoMonad[Tuple1]
    }
    object tuple2 {
      def show[A: Show, B: Show] = Show[(A, B)]
      def equal[A: Equal, B: Equal] = Equal[(A, B)]
      def order[A: Order, B: Order] = Order[(A, B)]
      def semigroup[A: Semigroup, B: Semigroup] = Semigroup[(A, B)]
      def monoid[A: Monoid, B: Monoid] = Monoid[(A, B)]
      def group[A: Group, B: Group] = Group[(A, B)]
      
      def bitraverse = BiTraverse[Tuple2]
      def functor = Functor[({type λ[α] = (B, α)})#λ]
      def monad[A: Monoid] = Functor[({type λ[α] = (A, α)})#λ]
    }
    object tuple3 {
      def show[A: Show, B: Show, C: Show] = Show[(A, B, C)]
      def equal[A: Equal, B: Equal, C: Equal] = Equal[(A, B, C)]
      def order[A: Order, B: Order, C: Order] = Order[(A, B, C)]
      def semigroup[A: Semigroup, B: Semigroup, C: Semigroup] = Semigroup[(A, B, C)]
      def monoid[A: Monoid, B: Monoid, C: Monoid] = Monoid[(A, B, C)]
      def group[A: Group, B: Group, C: Group] = Group[(A, B, C)]
    }
  }
}
