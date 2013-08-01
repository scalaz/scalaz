package scalaz

import scalacheck.ScalazArbitrary._
import syntax.equal._
import std.string._
import std.anyVal._
import std.stream._
import org.specs2.matcher.{Parameters, ExceptionMatchers, TraversableMatchers}

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
  */

  "concatenating ropes work correctly" ! prop {(t1: Rope[Int], t2: Rope[Int]) ⇒
    (!t1.isEmpty && !t2.isEmpty) ==>
      ((t1 ++ t2).toStream must be_===(t1.toStream ++ t2.toStream))
  }

  "chunk append works correctly" ! prop {(tree: Rope[Int], arr: ImmutableArray[Int]) ⇒
    (tree ::+ arr).toStream must be_===(tree.toStream ++ arr.toStream)
  }

  "chunk prepend works correctly" ! prop {(tree: Rope[Int], arr: ImmutableArray[Int]) ⇒
    (arr +:: tree).toStream must be_===(arr.toStream ++ tree.toStream)
  }

  "converting a stream to a finger-tree and back produces an equal stream" ! prop {(stream: Stream[Int]) =>
    streamToTree(stream).toStream must be_===(stream)
  }

//  "splitting a tree works the same as splitting a stream" ! prop {(tree: Rope[Int], index: Int) =>
//    val asStream = tree.toStream
//    val splitTree = tree.split(_ > index)
//    (splitTree._1.toStream, splitTree._2.toStream) === asStream.splitAt(index)
//  }

  "head works correctly" ! prop {(tree: Rope[Int]) ⇒
    !tree.isEmpty ==> (tree.head must be_===(tree.toStream.head))
  }

  "last works correctly" ! prop {(tree: Rope[Int]) ⇒
    !tree.isEmpty ==> (tree.last must be_===(tree.toStream.last))
  }

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

}
