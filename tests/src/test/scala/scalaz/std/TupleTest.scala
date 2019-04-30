package scalaz
package std

import scala.collection.immutable.IndexedSeq

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

object TupleTest extends SpecLite {

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

  checkAll("Tuple1", monoid.laws[(A)])
  checkAll("Tuple2", monoid.laws[(A, B)])
  checkAll("Tuple3", monoid.laws[(A, B, C)])
  checkAll("Tuple4", monoid.laws[(A, B, C, D)])
  checkAll("Tuple5", monoid.laws[(A, B, C, D, E)])
  checkAll("Tuple6", monoid.laws[(A, B, C, D, E, F)])
  checkAll("Tuple7", monoid.laws[(A, B, C, D, E, F, G)])
  checkAll("Tuple8", monoid.laws[(A, B, C, D, E, F, G, H)])

  checkAll("Tuple1", semilattice.laws[Tuple1[ISet[A]]])
  checkAll("Tuple2", semilattice.laws[(ISet[A], ISet[B])])
  checkAll("Tuple3", semilattice.laws[(ISet[A], ISet[B], ISet[C])])
  checkAll("Tuple4", semilattice.laws[(ISet[A], ISet[B], ISet[C], ISet[D])])
  checkAll("Tuple5", semilattice.laws[(ISet[A], ISet[B], ISet[C], ISet[D], ISet[E])])
  checkAll("Tuple6", semilattice.laws[(ISet[A], ISet[B], ISet[C], ISet[D], ISet[E], ISet[F])])
  checkAll("Tuple7", semilattice.laws[(ISet[A], ISet[B], ISet[C], ISet[D], ISet[E], ISet[F], ISet[G])])
  checkAll("Tuple8", semilattice.laws[(ISet[A], ISet[B], ISet[C], ISet[D], ISet[E], ISet[F], ISet[G], ISet[H])])

  checkAll("Tuple2", bindRec.laws[(B, ?)])
  checkAll("Tuple3", bindRec.laws[(B, C, ?)])
  checkAll("Tuple4", bindRec.laws[(B, C, D, ?)])
  checkAll("Tuple5", bindRec.laws[(B, C, D, E, ?)])
  checkAll("Tuple6", bindRec.laws[(B, C, D, E, F, ?)])
  checkAll("Tuple7", bindRec.laws[(B, C, D, E, F, G, ?)])
  checkAll("Tuple8", bindRec.laws[(B, C, D, E, F, G, H, ?)])

  checkAll("Tuple1", monad.laws[Tuple1])
  checkAll("Tuple2", monad.laws[(B, ?)])
  checkAll("Tuple3", monad.laws[(B, C, ?)])
  checkAll("Tuple4", monad.laws[(B, C, D, ?)])
  checkAll("Tuple5", monad.laws[(B, C, D, E, ?)])
  checkAll("Tuple6", monad.laws[(B, C, D, E, F, ?)])
  checkAll("Tuple7", monad.laws[(B, C, D, E, F, G, ?)])
  checkAll("Tuple8", monad.laws[(B, C, D, E, F, G, H, ?)])

  checkAll("Tuple1", comonad.laws[Tuple1])
  checkAll("Tuple2", comonad.laws[(Int, ?)])

  checkAll("Tuple2", associative.laws[Tuple2])

  "syntax" should {
    import std.tuple.tupleSyntax._
    "mapElements" in {
       (1, 2).mapElements(_1 = 2 *) must_===((2, 2))
    }
    "toIndexedSeq" in {
      val as: IndexedSeq[Int] = (1, 2).toIndexedSeq
      as.toList must_===(List(1, 2))
    }
    "fold" in {
      (1, 2).fold(_ + _) must_===(3)
    }
  }

  object instances {
    object tuple1 {
      def show[A: Show] = Show[Tuple1[A]]
      def equal[A: Equal] = Equal[Tuple1[A]]
      def order[A: Order] = Order[Tuple1[A]]
      def semigroup[A: Semigroup] = Semigroup[Tuple1[A]]
      def band[A: Band] = Band[Tuple1[A]]
      def monoid[A: Monoid] = Monoid[Tuple1[A]]

      def monad = Monad[Tuple1]
      def comonad = Comonad[Tuple1]
      def cozip = Cozip[Tuple1]

      // checking absence of ambiguity
      def equal[A: Order] = Equal[Tuple1[A]]
      def semigroup[A: Monoid: Band] = Semigroup[Tuple1[A]]
      def semigroup[A: Monoid: SemiLattice] = Semigroup[Tuple1[A]]
      def band[A: SemiLattice] = Band[Tuple1[A]]
    }
    object tuple2 {
      def show[A: Show, B: Show] = Show[(A, B)]
      def equal[A: Equal, B: Equal] = Equal[(A, B)]
      def order[A: Order, B: Order] = Order[(A, B)]
      def semigroup[A: Semigroup, B: Semigroup] = Semigroup[(A, B)]
      def band[A: Band, B: Band] = Band[(A, B)]
      def monoid[A: Monoid, B: Monoid] = Monoid[(A, B)]

      def associative = Associative[Tuple2]
      def bitraverse = Bitraverse[Tuple2]
      def functor = Functor[(B, ?)]
      def bindRec[A: Semigroup] = BindRec[(A, ?)]
      def monad[A: Monoid] = Monad[(A, ?)]
      def cozip = Cozip[(A, ?)]

      // checking absence of ambiguity
      def equal[A: Order, B: Order] = Equal[(A, B)]
      def semigroup[A: Monoid: Band, B: Monoid: Band] = Semigroup[(A, B)]
      def semigroup[A: Monoid: SemiLattice, B: Monoid: SemiLattice] = Semigroup[(A, B)]
      def band[A: SemiLattice, B: SemiLattice] = Band[(A, B)]
      def functor[A: Monoid] = Functor[(A, ?)]
    }
    object tuple3 {
      def show[A: Show, B: Show, C: Show] = Show[(A, B, C)]
      def equal[A: Equal, B: Equal, C: Equal] = Equal[(A, B, C)]
      def order[A: Order, B: Order, C: Order] = Order[(A, B, C)]
      def semigroup[A: Semigroup, B: Semigroup, C: Semigroup] = Semigroup[(A, B, C)]
      def band[A: Band, B: Band, C: Band] = Band[(A, B, C)]
      def monoid[A: Monoid, B: Monoid, C: Monoid] = Monoid[(A, B, C)]

      // checking absence of ambiguity
      def semigroup[A: Monoid: Band, B: Monoid: Band, C: Monoid: Band] = Semigroup[(A, B, C)]
      def semigroup[A: Monoid: SemiLattice, B: Monoid: SemiLattice, C: Monoid: SemiLattice] = Semigroup[(A, B, C)]
      def band[A: SemiLattice, B: SemiLattice, C: SemiLattice] = Band[(A, B, C)]
    }
  }
}
