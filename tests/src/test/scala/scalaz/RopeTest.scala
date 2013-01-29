package scalaz

import scalacheck.ScalazArbitrary._
import reflect.ClassManifest
import syntax.equal._
import std.string._
import std.anyVal._
import std.stream._
import org.specs2.control.LazyParameter
import collection.GenTraversable
import collection.immutable.Traversable
import org.specs2.matcher.{Parameters, ExceptionMatchers, TraversableMatchers, ContainInOrderMatcher, ContainMatcher}

class RopeTest extends Spec with ExceptionMatchers with TraversableMatchers {

  import Rope._

  //def beTheSameRopeSeq[A : ClassManifest] = containInOrder(_: Seq[A]) ^^ (wrapRope(_: Rope[A]))
  import scala.Predef.{implicitly => ?}

  // TODO get rid of code duplication
  implicit def SizeReducer[A]: Reducer[A, Int] = UnitReducer(x => 1)
  def streamToTree[A](stream: Stream[A]): FingerTree[Int, A] = stream.foldLeft(FingerTree.empty(SizeReducer[A])) {
    case (t, x) => (t :+ x)
  }

  override implicit val defaultParameters = Parameters(defaultValues.updated(maxSize, 25))

  "converting an array gives a rope of the same length" ! prop {(array: Array[Int]) =>
    Rope.fromArray(array).length must be_===(array.length)
  }

  "indexing a rope converted from an array is the same as indexing this array" ! prop {(array: Array[Int], i: Int) =>
    if (i >= 0 && i < array.length) Rope.fromArray(array).apply(i) must be_===(array(i))
    else Rope.fromArray(array).apply(i) must throwA[RuntimeException]
  }

  "length of a rope is the same as its length as a stream" ! prop {(rope: Rope[Int]) =>
    rope.length === rope.toStream.length
  }

  "indexing a rope is the same as converting it to a stream and indexing that" ! prop {(rope: Rope[Int], i: Int) =>
    if (i >= 0 && i < rope.length) rope(i) must be_===(rope.toStream(i))
    else rope(i) must throwA[RuntimeException]
  }

  "building a rope from chunks and converting it back is the same as filtering out empty chunks" ! prop {(chunks: List[ImmutableArray[Int]]) =>
    Rope.fromChunks(chunks).chunks.toList must_== chunks.filterNot(_.isEmpty)
  }

  "appending one element works correctly" ! prop {(tree: Rope[Int], x: Int) =>
    (tree :+ x).toStream must be_===(tree.toStream :+ x)
  }

  "prepending one element works correctly" ! prop {(tree: Rope[Int], x: Int) =>
    (x +: tree).toStream must be_===(x +: tree.toStream)
  }

  "StringLike instance" ! prop {(strings: List[String]) =>
    val rope = Rope.fromChunks(strings.map(ImmutableArray.fromString))
    rope.asString must be_===(strings.mkString)
  }

  /*"a rope converted to a stream is the same sequence as the original rope" ! prop {(rope: Rope[Int]) =>
     rope must beTheSameRopeSeq(rope.toStream)
  }.set(minTestsOk -> 15)
  
  "appending ropes works correctly" ! prop {(rope1: Rope[Int], rope2: Rope[Int]) =>
    (rope1 ++ rope2) must (haveClass[Rope[_]] and beTheSameRopeSeq(rope1.toStream ++ rope2.toStream))
  }.set(minTestsOk -> 15)*/


  "converting a stream to a finger-tree and back produces an equal stream" ! prop {(stream: Stream[Int]) =>
    streamToTree(stream).toStream must be_===(stream)
  }

//  "splitting a tree works the same as splitting a stream" ! prop {(tree: Rope[Int], index: Int) =>
//    val asStream = tree.toStream
//    val splitTree = tree.split(_ > index)
//    (splitTree._1.toStream, splitTree._2.toStream) === asStream.splitAt(index)
//  }

/*
  "init works correctly" ! prop {(tree: Rope[Int]) =>
    !tree.isEmpty ==> (tree.init.toStream must be_===(tree.toStream.init))
  }

  "tail works correctly" ! prop {(tree: Rope[Int]) =>
    !tree.isEmpty ==> (tree.tail.toStream must be_===(tree.toStream.tail))
  }

  "replacing last element works correctly" ! prop {(tree: Rope[Int], x: Int) =>
    !tree.isEmpty ==> ((tree.init :+ x).toStream must be_===(tree.toStream.init :+ x))
  }

  "replacing first element works correctly" ! prop {(tree: Rope[Int], x: Int) =>
    !tree.isEmpty ==> ((x +: tree.tail).toStream must be_===(x +: tree.toStream.tail))
  }
*/

//  "last and init work correctly" ! prop {(tree: Rope[Int]) =>
//    val asStream = tree.toStream
//    tree.isEmpty || ((tree.last === tree.toStream.last) && (tree.init.toStream === tree.toStream.init))
//  }
//
//  "viewl works correctly" ! prop {(tree: Rope[Int]) =>
//    val asStream = tree.toStream
//    tree.viewl.fold[Boolean](true, (x: Int, t: ({type λ[α]=FingerTree[Int, α]})#λ) => (x === asStream.head) && (t.toStream === asStream.tail))
//  }
//
//  "viewr works correctly" ! prop {(tree: Rope[Int]) =>
//    val asStream = tree.toStream
//    tree.viewr.fold[Boolean](true, (i: ({type λ[α]=FingerTree[Int, α]})#λ, x: Int) => (i.toStream === asStream.init) && (x === asStream.last))
//  }
}
