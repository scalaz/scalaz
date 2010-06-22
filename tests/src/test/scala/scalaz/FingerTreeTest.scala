package scalaz

import org.specs.{Sugar, Specification, ScalaCheck}
import org.scalacheck._
import Gen._
import Prop._
import Arbitrary._
import scalacheck.ScalaCheckBinding._
import Scalaz._
import FingerTree._
import scalacheck.ScalazArbitrary._

class FingerTreeTest extends Specification with Sugar with ScalaCheck {
  type SequenceTree[A] = FingerTree[Int, A]
  implicit def SizeReducer[A]: Reducer[A, Int] = Reducer(x => 1)
  def streamToTree[A](stream: Stream[A]): SequenceTree[A] = stream.foldl(FingerTree.empty(SizeReducer[A])) {
    case (t, x) => (t :+ x)
  }

  "appending one element works correctly" verifies {(tree: SequenceTree[Int], x: Int) =>
    (tree :+ x).toStream ≟ (tree.toStream :+ x)
  }

  "prepending one element works correctly" verifies {(tree: SequenceTree[Int], x: Int) =>
    (x +: tree).toStream ≟ (x +: tree.toStream)
  }

  "converting a stream to a finger-tree and back produces an equal stream" verifies {(stream: Stream[Int]) =>
    streamToTree(stream).toStream ≟ stream
  }

  "appending two trees works correctly" verifies {(tree1: SequenceTree[Int], tree2: SequenceTree[Int]) =>
    (tree1 <++> tree2).toStream ≟ (tree1.toStream ++ tree2.toStream)
  }

  "splitting a tree works the same as splitting a stream" verifies {(tree: SequenceTree[Int], index: Int) =>
    val asStream = tree.toStream
    val splitTree = tree.split(_ > index)
    (splitTree._1.toStream, splitTree._2.toStream) ≟ asStream.splitAt(index)
  }
}
