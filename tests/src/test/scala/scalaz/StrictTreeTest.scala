package scalaz

import std.AllInstances._
import scalaz.scalacheck.ScalazProperties._
import scalaz.scalacheck.ScalazArbitrary._
import StrictTree._
import org.scalacheck.Prop.forAll

object StrictTreeTest extends SpecLite {

  checkAll("StrictTree", order.laws[StrictTree[Int]])
  checkAll("StrictTree", traverse1.laws[StrictTree])
  checkAll("StrictTree", applicative.laws[StrictTree])
  checkAll("StrictTree", comonad.laws[StrictTree])
  checkAll("StrictTree", align.laws[StrictTree])
  checkAll("StrictTree", zip.laws[StrictTree])

  checkAll(FoldableTests.anyAndAllLazy[Tree])

  "indexed" ! forAll { xs: StrictTree[Byte] =>
    val F = Traverse[StrictTree]
    val a = F.indexed(xs)
    Equal[StrictTree[Byte]].equal(a.map(_._2), xs) must_=== true
    F.toList(a) must_=== F.toList(xs).zipWithIndex.map{case (a, b) => (b, a)}
  }

  "A tree must can be rendered as an ASCII string" ! {
    Node(1, Vector(Node(2, Vector(Leaf(3))), Leaf(4))).drawTree must_== Seq(
      "1",
      "|",
      "+- 2",
      "|  |",
      "|  `- 3",
      "|",
      "`- 4").mkString("", "\n", "\n")
  }

  "Equals.equal works" ! forAll { (s: StrictTree[Byte]) =>
    val E = Equal[StrictTree[Byte]]
    E.equal(s, s) must_== true
  }

  "flatMap((Leaf(_)) is identity" ! forAll { (s: StrictTree[Byte]) =>
    val actualTree = s.flatMap(Leaf(_))
    Equal[StrictTree[Byte]].equal(actualTree, s) must_== true
  }

  "flatten is the same as the lazy Tree's flatten" ! forAll { (s: StrictTree[Byte]) =>
    s.flatten must_=== s.toTree.flatten.toVector
  }

  "StrictTree#toTree and Tree#toStrictTree are inverses" ! forAll { (s: StrictTree[Byte]) =>
    Equal[StrictTree[Byte]].equal(s, s.toTree.toStrictTree) must_=== true
  }

  "size gives the same number of elements as flatten" ! forAll { (s: StrictTree[Byte]) =>
    s.flatten.size must_=== s.size
  }

}
