package scalaz

import Scalaz._
import scalacheck.ScalazArbitrary._
import org.specs.{ScalaCheck, Specification}
import org.specs.{Sugar}
class RopeTest extends Specification with Sugar with ScalaCheck {
  "converting an array gives a rope of the same length" verifies {(array: Array[Int]) =>
    Rope.fromArray(array).length ≟ array.length
  }

  "indexing a rope converted from an array is the same as indexing this array" verifies {(array: Array[Int], i: Int) =>
    // Rope.fromArray(array).apply(i) must (beEqualTo(array(i)) or throwA[RuntimeException])
    if (i >= 0 && i < array.length) (Rope.fromArray(array).apply(i) must (beEqualTo(array(i))))
    else (Rope.fromArray(array).apply(i) must throwA[RuntimeException])
  }

  "length of a rope is the same as its length as a stream" verifies {(rope: Rope[Int]) =>
    rope.length ≟ rope.toStream.length
  }

  "indexing a rope is the same as converting it to a stream and indexing that" verifies {(rope: Rope[Int], i: Int) =>
    // Rope.fromArray(array).apply(i) must (beEqualTo(array(i)) or throwA[RuntimeException])
    if (i >= 0 && i < rope.length) (rope(i) must (beEqualTo(rope.toStream(i))))
    else (rope(i) must throwA[RuntimeException])
  }

//  def streamToTree[A](stream: Stream[A]): Rope[A] = stream.foldl(FingerTree.empty(SizeReducer[A])) {
//    case (t, x) => (t :+ x)
//  }
//
//  "appending one element works correctly" verifies {(tree: Rope[Int], x: Int) =>
//    (tree :+ x).toStream ≟ (tree.toStream :+ x)
//  }
//
//  "prepending one element works correctly" verifies {(tree: Rope[Int], x: Int) =>
//    (x +: tree).toStream ≟ (x +: tree.toStream)
//  }
//
//  "converting a stream to a finger-tree and back produces an equal stream" verifies {(stream: Stream[Int]) =>
//    streamToTree(stream).toStream ≟ stream
//  }

//  "appending two trees works correctly" verifies {(tree1: Rope[Int], tree2: Rope[Int]) =>
//    (tree1 <++> tree2).toStream ≟ (tree1.toStream ++ tree2.toStream)
//  }
//
//  "splitting a tree works the same as splitting a stream" verifies {(tree: Rope[Int], index: Int) =>
//    val asStream = tree.toStream
//    val splitTree = tree.split(_ > index)
//    (splitTree._1.toStream, splitTree._2.toStream) ≟ asStream.splitAt(index)
//  }
//
//  "replacing last element works correctly" verifies {(tree: Rope[Int], x: Int) =>
//    tree.isEmpty || ((tree :-| x).toStream ≟ (tree.toStream.init :+ x))
//  } // can't use conditional property here, it would be better to write !tree.isEmpty ==> ...
//
//  "replacing first element works correctly" verifies {(tree: Rope[Int], x: Int) =>
//    tree.isEmpty || ((x |-: tree).toStream ≟ (x +: tree.toStream.tail))
//  }

//  "head and tail work correctly" verifies {(tree: Rope[Int]) =>
//    val asStream = tree.toStream
//    tree.isEmpty || ((tree.head ≟ tree.toStream.head) && (tree.tail.toStream ≟ tree.toStream.tail))
//  }
//
//  "last and init work correctly" verifies {(tree: Rope[Int]) =>
//    val asStream = tree.toStream
//    tree.isEmpty || ((tree.last ≟ tree.toStream.last) && (tree.init.toStream ≟ tree.toStream.init))
//  }

//  "viewl works correctly" verifies {(tree: Rope[Int]) =>
//    val asStream = tree.toStream
//    tree.viewl.fold[Boolean](true, (x: Int, t: PartialApply1Of2[FingerTree, Int]#Apply) => (x ≟ asStream.head) && (t.toStream ≟ asStream.tail))
//  }
//
//  "viewr works correctly" verifies {(tree: Rope[Int]) =>
//    val asStream = tree.toStream
//    tree.viewr.fold[Boolean](true, (i: PartialApply1Of2[FingerTree, Int]#Apply, x: Int) => (i.toStream ≟ asStream.init) && (x ≟ asStream.last))
//  }
}