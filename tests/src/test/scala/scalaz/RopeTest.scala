package scalaz

import scalaz.scalacheck.ScalazProperties._
import scalacheck.ScalazArbitrary._
import syntax.equal._
import std.string._
import std.anyVal._
import std.stream._
import std.option._
import org.scalacheck.Prop.forAll

object RopeTest extends SpecLite {

  import Rope._

  checkAll(equal.laws[Rope[Int]])
  checkAll(foldable.laws[Rope])
  checkAll(plus.laws[Rope])

  // TODO get rid of code duplication
  def streamToRope[A : ClassManifest](stream: Stream[A]): Rope[A] = stream.foldLeft(Rope.empty[A]) {
    case (t, x) => (t :+ x)
  }

  //override implicit val defaultParameters = Parameters(defaultValues.updated(maxSize, 25))

  "a empty rope doesn't have any elements" in {
    Rope.empty[Int].length must_===(0)
  }

  "a rope consturcted from a finger tree should have the same length" ! forAll {v: FingerTreeIntPlus[ImmutableArray[Int]] =>
    Rope.apply(v).length must_===(v.toList.flatten.length)
  }

  "indexing a rope created from a finger tree should be the same as indexing this tree" ! forAll {(v: FingerTreeIntPlus[ImmutableArray[Int]], i: Int) =>
    val flat = v.toList.flatten
    if (i >= 0 && i < flat.length) Rope(v).apply(i) must_===(flat(i))
    else Rope(v).apply(i).mustThrowA[RuntimeException]
  }

  "converting an array gives a rope of the same length" ! forAll {(array: Array[Int]) =>
    Rope.fromArray(array).length must_===(array.length)
  }

  "indexing a rope converted from an array is the same as indexing this array" ! forAll {(array: Array[Int], i: Int) =>
    if (i >= 0 && i < array.length) Rope.fromArray(array).apply(i) must_===(array(i))
    else Rope.fromArray(array).apply(i).mustThrowA[RuntimeException]
  }

  "converting an string gives a rope of the same length" ! forAll {(string: String) =>
    Rope.fromString(string).length must_===(string.length)
  }

  "indexing a rope converted from a string is the same as indexing this string" ! forAll {(string: String, i: Int) =>
    if (i >= 0 && i < string.length) Rope.fromString(string).apply(i) must_===(string.charAt(i))
    else Rope.fromString(string).apply(i).mustThrowA[RuntimeException]
  }

  "length of a rope is the same as its length as a stream" ! forAll {(rope: Rope[Int]) =>
    rope.length === rope.toStream.length
  }

  "indexing a rope is the same as converting it to a stream and indexing that" ! forAll {(rope: Rope[Int], i: Int) =>
    if (i >= 0 && i < rope.length) rope(i) must_===(rope.toStream(i))
    else rope(i).mustThrowA[RuntimeException]
  }

  "building a rope from chunks and converting it back is the same as filtering out empty chunks" ! forAll {(chunks: List[ImmutableArray[Int]]) =>
    Rope.fromChunks(chunks).chunks.toList must_== chunks.filterNot(_.isEmpty)
  }

  "appending one element works correctly" ! forAll {(tree: Rope[Int], x: Int) =>
    (tree :+ x).toStream must_===(tree.toStream :+ x)
  }

  "prepending one element works correctly" ! forAll {(tree: Rope[Int], x: Int) =>
    (x +: tree).toStream must_===(x +: tree.toStream)
  }

  "StringLike instance" ! forAll {(strings: List[String]) =>
    val rope = Rope.fromChunks(strings.map(ImmutableArray.fromString))
    rope.asString must_===(strings.mkString)
  }

  /*"a rope converted to a stream is the same sequence as the original rope" ! forAll {(rope: Rope[Int]) =>
     rope must beTheSameRopeSeq(rope.toStream)
  }.set(minTestsOk -> 15)
  */

  "iterator should have the same length as its rope" ! forAll { rope: Rope[Int] =>
    rope.iterator.length must_===(rope.length)
  }

  "iterator should have the same elements as its rope" ! forAll { rope: Rope[Int] =>
    val elems = rope.iterator.toList
    for (i <- 0 until rope.size) elems(i) must_===(rope(i))
  }

  "reverse iterator should have the same length as its rope" ! forAll { rope: Rope[Int] =>
    rope.reverseIterator.length must_===(rope.length)
  }

  "reverse iterator should have the same elements as its rope in reverse order" ! forAll { rope: Rope[Int] =>
    val elems = rope.reverseIterator.toList.reverse
    for (i <- 0 until rope.size) elems(i) must_===(rope(i))
  }

  "concatenating ropes work correctly" ! forAll {(t1: Rope[Int], t2: Rope[Int]) ⇒
    (!t1.isEmpty && !t2.isEmpty) ==>
      ((t1 ++ t2).toStream must_===(t1.toStream ++ t2.toStream))
  }

  "chunk append works correctly" ! forAll {(tree: Rope[Int], arr: ImmutableArray[Int]) ⇒
    (tree ::+ arr).toStream must_===(tree.toStream ++ arr.toStream)
  }

  "chunk prepend works correctly" ! forAll {(tree: Rope[Int], arr: ImmutableArray[Int]) ⇒
    (arr +:: tree).toStream must_===(arr.toStream ++ tree.toStream)
  }

  "converting a stream to a rope and back produces an equal stream" ! forAll {(stream: Stream[Int]) =>
    streamToRope(stream).toStream must_===(stream)
  }

//  "splitting a tree works the same as splitting a stream" ! forAll {(tree: Rope[Int], index: Int) =>
//    val asStream = tree.toStream
//    val splitTree = tree.split(_ > index)
//    (splitTree._1.toStream, splitTree._2.toStream) === asStream.splitAt(index)
//  }

  "head works correctly" ! forAll {(tree: Rope[Int]) ⇒
    !tree.isEmpty ==> (tree.head must_===(tree.toStream.head))
  }

  "last works correctly" ! forAll {(tree: Rope[Int]) ⇒
    !tree.isEmpty ==> (tree.last must_===(tree.toStream.last))
  }

  "init works correctly" ! forAll {(tree: Rope[Int]) =>
    !tree.isEmpty ==> (tree.init.toStream must_===(tree.toStream.init))
  }

  "tail works correctly" ! forAll {(tree: Rope[Int]) =>
    !tree.isEmpty ==> (tree.tail.toStream must_===(tree.toStream.tail))
  }

   "replacing last element works correctly" ! forAll {(tree: Rope[Int], x: Int) =>
    !tree.isEmpty ==> ((tree.init :+ x).toStream must_===(tree.toStream.init :+ x))
  }

  "replacing first element works correctly" ! forAll {(tree: Rope[Int], x: Int) =>
    !tree.isEmpty ==> ((x +: tree.tail).toStream must_===(x +: tree.toStream.tail))
  }

  "get" ! forAll { (tree: Rope[Int], i: Int) =>
    if(0 <= i && i < tree.length)
      tree.get(i) must_===(Some(tree.self.toList.flatten.apply(i)))
    else
      tree.get(i) must_===(None)
  }
}
