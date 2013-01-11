package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import Tree._

class TreeTest extends Spec {

  checkAll("Tree", equal.laws[Tree[Int]])

  {
    implicit def treeEqual[A: Equal]: Equal[Tree[A]] = new Equal[Tree[A]] {
      import std.stream.streamEqual
      def streamEqualApprox = streamEqual[Tree[A]].contramap((_: Stream[Tree[A]]).take(1000))
      def equal(a1: Tree[A], a2: Tree[A]) =
        Equal[A].equal(a1.rootLabel, a2.rootLabel) && streamEqualApprox.equal(a1.subForest, a2.subForest)
    }

    // TODO checkAll("Tree", traverse.laws[Tree])
    checkAll("Tree", applicative.laws[Tree])
    checkAll("Tree", comonad.laws[Tree])
  }

  "A tree must can be rendered as an ASCII string" >> {
      node(1, Stream(node(2, Stream(leaf(3))), leaf(4))).drawTree must_== Seq(
      "1",
      "|",
      "+- 2",
      "|  |",
      "|  `- 3",
      "|",
      "`- 4").mkString("", "\n", "\n")
  }
}
