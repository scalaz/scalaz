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

  "Issue #1001" ! {
    val x = 100000 // Should be large enough to induce a stack overflow
    Tree.node(0, (1 to x).toStream.map(i => Tree.leaf(i)))
      .flatten must_== (0 to x).toStream
  }
}
