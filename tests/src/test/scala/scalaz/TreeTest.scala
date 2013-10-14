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
