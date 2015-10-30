package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import Tree._
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll

object TreeTest extends SpecLite {

  checkAll("Tree", order.laws[Tree[Int]])
  checkAll("Tree", traverse1.laws[Tree])
  checkAll("Tree", applicative.laws[Tree])
  checkAll("Tree", comonad.laws[Tree])
  checkAll("Tree", align.laws[Tree])
  checkAll("Tree", zip.laws[Tree])

  checkAll(FoldableTests.anyAndAllLazy[Tree])

  "ScalazArbitrary.treeGenSized" ! forAll(Gen.choose(1, 200)){ size =>
    val gen = treeGenSized[Unit](size)
    Stream.continually(gen.sample).flatten.take(10).map(Foldable[Tree].length(_)).forall(_ == size)
  }

  "infinite Tree flatten" ! {
    Node(0, Stream.from(1).map(Leaf(_))).flatten
    true
  }

  "deep Tree flatten should not cause a stack overflow" ! {
    val size = 1000000
    val tree = (1 to size).foldLeft(Leaf(0))((x, y) => Node(y, Stream(x)))
    tree.flatten must_== (size to 0 by -1).toStream
  }

  "A tree must can be rendered as an ASCII string" ! {
      Node(1, Stream(Node(2, Stream(Leaf(3))), Leaf(4))).drawTree must_== Seq(
      "1",
      "|",
      "+- 2",
      "|  |",
      "|  `- 3",
      "|",
      "`- 4").mkString("", "\n", "\n")
  }
}
