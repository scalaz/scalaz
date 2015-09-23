package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import Tree._
import org.scalacheck.Prop.forAll

object TreeTest extends SpecLite {

  checkAll("Tree", order.laws[Tree[Int]])

  checkAll(FoldableTests.anyAndAllLazy[Tree])

  "satisfy foldable1 law" ! forAll { xs: Tree[Int] =>
    val F = Foldable1[Tree]
    F.foldMap1(xs)(Vector(_)) must_===(F.foldRight1(xs.map(Vector(_)))((a, b) => b ++ a))
    F.foldMap1(xs)(Vector(_)) must_===(F.foldLeft1(xs.map(Vector(_)))(_ ++ _))
  }

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

  "infinite Tree flatten" ! {
    Tree.node(0, Stream.from(1).map(Tree.leaf(_))).flatten
    true
  }

  "deep Tree flatten should not cause a stack overflow" ! {
    val size = 1000000
    val tree = (1 to size).foldLeft(leaf(0))((x, y) => node(y, Stream(x)))
    tree.flatten must_== (size to 0 by -1).toStream
  }

  "A tree must can be rendered as an ASCII string" ! {
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
