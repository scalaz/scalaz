package scalaz

import org.scalacheck.Prop._
import FingerTree._
import scalacheck.ScalazArbitrary._
import scalacheck.ScalazProperties._
import std.anyVal._
import std.stream._
import std.string._
import std.tuple._
import std.option._
import syntax.applicative._
import syntax.equal._
import WriterT._

class FingerTreeTest extends Spec {
  type SequenceTree[A] = FingerTree[Int, A]
  implicit def SizeReducer[A]: Reducer[A, Int] = UnitReducer(x => 1)

  checkAll(monoid.laws[SequenceTree[String]])

  val intStream = Stream.from(1)

  def streamToTree[A](stream: Stream[A]): SequenceTree[A] = stream.foldLeft(FingerTree.empty(SizeReducer[A])) {
    case (t, x) => (t :+ x)
  }

  "append one element works correctly" ! prop {(tree: SequenceTree[Int], x: Int) =>
    (tree :+ x).toStream must be_===(tree.toStream :+ x)
  }

  "prepending one element works correctly" ! prop {(tree: SequenceTree[Int], x: Int) =>
    (x +: tree).toStream must be_===(x +: tree.toStream)
  }

  "converting a stream to a finger-tree and back produces an equal stream" ! prop {(stream: Stream[Int]) =>
    streamToTree(stream).toStream must be_===(stream)
  }

  "appending two trees works correctly" ! prop {(tree1: SequenceTree[Int], tree2: SequenceTree[Int]) =>
    (tree1 <++> tree2).toStream must be_===(tree1.toStream ++ tree2.toStream)
  }

  "splitting a tree works the same as splitting a stream" ! prop {(tree: SequenceTree[Int], index: Int) =>
    val asStream = tree.toStream
    val splitTree = tree.split(_ > index)
    (splitTree._1.toStream, splitTree._2.toStream) must be_===(asStream.splitAt(index))
  }

  "replacing last element works correctly" ! check{(tree: SequenceTree[Int], x: Int) =>
    !tree.isEmpty ==> ((tree :-| x).toStream must be_===(tree.toStream.init :+ x))
  }

  "replacing first element works correctly" ! prop {(tree: SequenceTree[Int], x: Int) =>
    !tree.isEmpty ==> ((x |-: tree).toStream must be_=== (x +: tree.toStream.tail))
  }

  "head and tail work correctly"  ! prop {(tree: SequenceTree[Int]) =>
    val asStream = tree.toStream
    !tree.isEmpty ==> ((tree.head === tree.toStream.head) && (tree.tail.toStream === tree.toStream.tail))
  }

  "last and init work correctly" ! prop {(tree: SequenceTree[Int]) =>
    val asStream = tree.toStream
    !tree.isEmpty ==> ((tree.last === tree.toStream.last) && (tree.init.toStream === tree.toStream.init))
  }

  "foldLeft snoc is identity" ! prop {(tree: SequenceTree[Int]) => tree.foldLeft(FingerTree.empty(SizeReducer[Int]))(_ :+ _).toStream ?= tree.toStream}

  "foldLeft cons is reverse" ! prop {(tree: SequenceTree[Int]) => tree.foldLeft(FingerTree.empty(SizeReducer[Int]))((x, y) => y +: x).toStream ?= tree.toStream.reverse}

  "Fingertree" should {

    "apply effects in order" in {
      val s: Writer[String, FingerTree[Int, Int]] = streamToTree(intStream.take(5)).traverseTree[({type λ[α] = Writer[String, α]})#λ, Int, Int](x => Writer(x.toString, x))
      s.run must be_===("12345", streamToTree(intStream.take(5)))
    }

    "traverseTree through the option effect yielding result" in {
      val tree = streamToTree(intStream.take(20)).traverseTree[Option, Int, Int](i => Some(i * 2))
      tree.map(_.toStream) getOrElse(Stream.empty) must be_===(streamToTree(intStream.take(20).map(_ * 2)).toStream)
    }

    "traverseTree through the option effect yielding none" in {
      val tree = streamToTree(intStream.take(20)).traverseTree[Option, Int, Int](i => if (i < 10) Some(i * 2) else None)
      tree must be_===(None)
    }

    "not blow the stack" in {
      val tree: Option[FingerTree[Int, Int]] = streamToTree(intStream.take(32 * 1024)).traverseTree[Option, Int, Int](x => Some(x))
      tree.map(_.toStream.take(100)) getOrElse Stream.empty must be_===(intStream.take(100))
    }

  }

  "OrdSeq is ordered" ! prop { xs: List[Int] => OrdSeq(xs:_*).toList == xs.sorted }
  
  "IndSeq" should {
    import org.scalacheck._
    import Gen._
    import Arbitrary.arbitrary

    case class TestInstance(arr: Array[Int], index: Int)

    implicit def myGen: Arbitrary[TestInstance] = Arbitrary(for {
      arr <- arbitrary[Array[Int]] if arr.nonEmpty
      m <- Gen.choose(0, arr.length-1)
    } yield TestInstance(arr,m))

    "have a length" ! prop { xs: Array[Int] => IndSeq(xs: _*).length == xs.length }

    "allow random access" ! prop { ti: TestInstance =>
      IndSeq(ti.arr: _*)(ti.index) == ti.arr(ti.index)
    }
  }

  "viewl works correctly" ! prop {(tree: SequenceTree[Int]) =>
    val asStream = tree.toStream
    tree.viewl.fold[Boolean](true, (x, t) => (x === asStream.head) && (t.toStream === asStream.tail))
  }

  "viewr works correctly" ! prop {(tree: SequenceTree[Int]) =>
    val asStream = tree.toStream
    tree.viewr.fold[Boolean](true, (i, x) => (i.toStream ≟ asStream.init) && (x ≟ asStream.last))
  }
}
