package scalaz

import Scalaz._
import scalacheck.ScalazArbitrary._
import org.specs.{ScalaCheck, Specification, Sugar}
import reflect.ClassManifest

class RopeTest extends Specification with Sugar with ScalaCheck {
  def beTheSameSeqAsForRope[A : ClassManifest] = beTheSameSeqAs(_: Seq[A]) ^^ (wrapRope(_: Rope[A]))
  // import scala.Predef.{implicitly => ?}
  def m[A](implicit man: ClassManifest[A]) = man

  "converting an array gives a rope of the same length" verifies {(array: Array[Int]) =>
    Rope.fromArray(array).length ≟ array.length
  }

  "indexing a rope converted from an array is the same as indexing this array" verifies {(array: Array[Int], i: Int) =>
    if (i >= 0 && i < array.length) (Rope.fromArray(array).apply(i) must (beEqualTo(array(i))))
    else (Rope.fromArray(array).apply(i) must throwA[RuntimeException])
  }

  "length of a rope is the same as its length as a stream" verifies {(rope: Rope[Int]) =>
    rope.length ≟ rope.toStream.length
  }

  "indexing a rope is the same as converting it to a stream and indexing that" verifies {(rope: Rope[Int], i: Int) =>
    if (i >= 0 && i < rope.length) (rope(i) must (beEqualTo(rope.toStream(i))))
    else (rope(i) must throwA[RuntimeException])
  }

  "building a rope from chunks and converting it back is the same as filtering out empty chunks" verifies {(chunks: List[ImmutableArray[Int]]) =>
    Rope.fromChunks(chunks).chunks must beTheSameSeqAs(chunks.filterNot(_.isEmpty))
  }

  "a rope converted to a stream is the same sequence as the original rope" verifies {(rope: Rope[Int]) =>
    rope must beTheSameSeqAsForRope(m[Int])(rope.toStream)
  } set(minTestsOk -> 15)

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

  "appending ropes works correctly" verifies {(rope1: Rope[Int], rope2: Rope[Int]) =>
    (rope1 ++ rope2) must (haveClass[Rope[_]] and beTheSameSeqAsForRope(m[Int])(rope1.toStream ++ rope2.toStream))
  } set(minTestsOk -> 15)
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
//    tree.viewl.fold[Boolean](true, (x: Int, t: ({type λ[α]=FingerTree[Int, α]})#λ) => (x ≟ asStream.head) && (t.toStream ≟ asStream.tail))
//  }
//
//  "viewr works correctly" verifies {(tree: Rope[Int]) =>
//    val asStream = tree.toStream
//    tree.viewr.fold[Boolean](true, (i: ({type λ[α]=FingerTree[Int, α]})#λ, x: Int) => (i.toStream ≟ asStream.init) && (x ≟ asStream.last))
//  }

  "StringLike instance" verifies {(strings: List[String]) =>
    val rope = Rope.fromChunks(strings.map(ImmutableArray.fromString))
    rope.asString must beEqualTo(strings.mkString)}
}