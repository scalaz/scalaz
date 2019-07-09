package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import Tree._
import org.scalacheck.Prop.forAll

object TreeTest extends SpecLite {

  checkAll("Tree", order.laws[Tree[Int]])
  checkAll("Tree", traverse1.laws[Tree])
  checkAll("Tree", applicative.laws[Tree])
  checkAll("Tree", comonad.laws[Tree])
  checkAll("Tree", align.laws[Tree])
  checkAll("Tree", zip.laws[Tree])

  checkAll(FoldableTests.anyAndAllLazy[Tree])

  "indexed" ! forAll { xs: Tree[Byte] =>
    val F = Traverse[Tree]
    val a = F.indexed(xs)
    Equal[Tree[Byte]].equal(a.map(_._2), xs) must_=== true
    F.toList(a) must_=== F.toList(xs).zipWithIndex.map{case (a, b) => (b, a)}
  }

  "infinite Tree flatten" ! {
    Node(0, EphemeralStream.fromStream(Stream.from(1)).map(Leaf(_))).flatten
    true
  }

  "A tree must can be rendered as an ASCII string" ! {
      Node(1, EphemeralStream(Node(2, EphemeralStream(Leaf(3))), Leaf(4))).drawTree must_== Seq(
      "1",
      "|",
      "+- 2",
      "|  |",
      "|  `- 3",
      "|",
      "`- 4").mkString("", "\n", "\n")
  }
}
