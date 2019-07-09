package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._

object TreeLocTest extends SpecLite {

  checkAll("TreeLoc", order.laws[TreeLoc[Int]])
  checkAll("TreeLoc", traverse1.laws[TreeLoc])
  checkAll(FoldableTests.anyAndAllLazy[TreeLoc])

  {
    def treeEqual[A: Equal]: Equal[Tree[A]] = new Equal[Tree[A]] {
      // import std.stream.streamEqual
      import EphemeralStream.{EStream, ephemeralStreamEqual}

      def streamEqualApprox = ephemeralStreamEqual[Tree[A]].contramap((_: EStream[Tree[A]]).take(1000))
      def equal(a1: Tree[A], a2: Tree[A]) =
        Equal[A].equal(a1.rootLabel, a2.rootLabel) && streamEqualApprox.equal(a1.subForest, a2.subForest)
    }

    // TODO checkAll("TreeLoc", applicative.laws[TreeLoc])
    checkAll("TreeLoc", comonad.laws[TreeLoc])
  }

  "TreeLoc from empty forest does not throw an exception" ! {
    import scalaz.std.option._
    val result: Option[TreeLoc[Int]] = TreeLoc.fromForest(EphemeralStream.emptyEphemeralStream[Tree[Int]])
    result must_==(none[TreeLoc[Int]])
  }

  object instances {
    def equal[A: Equal] = Equal[TreeLoc[A]]
    def order[A: Order] = Order[TreeLoc[A]]

    // checking absence of ambiguity
    def equal[A: Order] = Equal[TreeLoc[A]]
  }
}
