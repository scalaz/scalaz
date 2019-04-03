package scalaz

import scalaz.scalacheck.ScalazArbitrary._
import StrictTree._
import org.scalacheck.Gen
import org.scalacheck.Prop.forAll
import std.AllInstances._

object StrictTreeTestJVM extends SpecLite {

  val E = Equal[StrictTree[Int]]

  "ScalazArbitrary.strictTreeGenSized" ! forAll(Gen.choose(1, 200)){ size =>
    val gen = strictTreeGenSized[Unit](size)
    Stream.continually(gen.sample).flatten.take(10).map(Foldable[StrictTree].length(_)).forall(_ == size)
  }

  def genTree(size: Int): StrictTree[Int] =
    (1 to size).foldLeft(Leaf(0))((x, y) => Node(y, Vector(x)))

  val size = 100000

  val deepTree = genTree(size)

  "deep foldMap should not cause a stack overflow" ! {
    deepTree.foldMap(identity)
    true
  }

  "deep foldRight should not cause a stack overflow" ! {
    deepTree.foldRight[Int](0)(_ + _)
    true
  }

  "deep flatten should not cause a stack overflow" ! {
    deepTree.flatten
    true
  }

  "deep levels should not cause a stack overflow" ! {
    deepTree.levels
    true
  }

  "deep scanr should not cause a stack overflow" ! {
    def f(a: Int, b: Seq[StrictTree[Int]]): Int = a + b.size
    deepTree.scanr[Int](f _)
    true
  }

  "deep size should not cause a stack overflow" ! {
    deepTree.size
    true
  }

  "deep Equal.equal should not cause a stack overflow" ! {
    E.equal(deepTree, deepTree)
  }

  "deep hashCode should not cause a stack overflow" ! {
    deepTree.hashCode
    true
  }

  "deep equals should not cause a stack overflow" ! {
    deepTree.equals(deepTree)
  }

  "deep toTree should not cause a stack overflow" ! {
    deepTree.toTree
    true
  }

  "deep map should not cause a stack overflow" ! {
    deepTree.map(_ + 1)
    true
  }

  "deep flatMap should not cause a stack overflow" ! {
    deepTree.flatMap(Leaf(_))
    true
  }

  "deep align should not cause a stack overflow" ! {
    Align[StrictTree].align(deepTree, deepTree)
    true
  }

  "deep zip should not cause a stack overflow" ! {
    Zip[StrictTree].zip(deepTree, deepTree)
    true
  }

  "deep unzip should not cause a stack overflow" ! {
    Zip[StrictTree].zip(deepTree, deepTree).unzip
    true
  }

}
