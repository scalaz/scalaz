package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import Tree._
import org.scalacheck.Prop.forAll

object TreeTest extends SpecLite {

  checkAll("Tree", equal.laws[Tree[Int]])
  checkAll("Tree", traverse1.laws[Tree])
  checkAll("Tree", applicative.laws[Tree])
  checkAll("Tree", comonad.laws[Tree])
  checkAll("Tree", align.laws[Tree])

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
