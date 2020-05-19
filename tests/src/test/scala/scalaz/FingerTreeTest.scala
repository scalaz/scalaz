package scalaz

import scalacheck.ScalazArbitrary._
import scalacheck.ScalazProperties._
import std.anyVal._
import std.lazylist._
import std.string._
import std.tuple._
import std.option._
import syntax.equal._
import WriterT._
import org.scalacheck.Prop.forAll

object FingerTreeTest extends SpecLite {
  type SequenceTree[A] = FingerTree[Int, A]
  implicit def SizeReducer[A]: Reducer[A, Int] = UnitReducer(x => 1)
  import FingerTree._

  checkAll(monoid.laws[SequenceTree[String]])
  checkAll(equal.laws[SequenceTree[String]])
  checkAll("FingerTree", foldable.laws[SequenceTree])
  checkAll("Finger", foldable.laws[Finger[Int, *]])
  checkAll("Node", foldable.laws[Node[Int, *]])

  checkAll("IndSeq", equal.laws[IndSeq[Int]])
  checkAll("IndSeq", monadPlus.strongLaws[IndSeq])
  checkAll("IndSeq", traverse.laws[IndSeq])
  checkAll("IndSeq", isEmpty.laws[IndSeq])
  checkAll("IndSeq", alt.laws[IndSeq])

  val intLazyList = LazyList.from(1)

  def lazyListToTree[A](list: LazyList[A]): SequenceTree[A] = list.foldLeft(FingerTree.empty[Int, A]) {
    case (t, x) => (t :+ x)
  }

  "append one element works correctly" ! forAll {(tree: SequenceTree[Int], x: Int) =>
    (tree :+ x).toLazyList must_===(tree.toLazyList :+ x)
  }

  "prepending one element works correctly" ! forAll {(tree: SequenceTree[Int], x: Int) =>
    (x +: tree).toLazyList must_===(x +: tree.toLazyList)
  }

  "converting a stream to a finger-tree and back produces an equal stream" ! forAll {(stream: LazyList[Int]) =>
    lazyListToTree(stream).toLazyList must_===(stream)
  }

  "appending two trees works correctly" ! forAll {(tree1: SequenceTree[Int], tree2: SequenceTree[Int]) =>
    (tree1 <++> tree2).toLazyList must_===(tree1.toLazyList ++ tree2.toLazyList)
  }

  "splitting a tree works the same as splitting a stream" ! forAll {(tree: SequenceTree[Int], index: Int) =>
    val asLazyList = tree.toLazyList
    val splitTree = tree.split(_ > index)
    (splitTree._1.toLazyList, splitTree._2.toLazyList) must_===(asLazyList.splitAt(index))
  }

  "replacing last element works correctly" ! forAll{(tree: SequenceTree[Int], x: Int) =>
    !tree.isEmpty ==> ((tree :-| x).toLazyList must_===(tree.toLazyList.init :+ x))
  }

  "replacing first element works correctly" ! forAll {(tree: SequenceTree[Int], x: Int) =>
    !tree.isEmpty ==> ((x |-: tree).toLazyList must_=== (x +: tree.toLazyList.tail))
  }

  "head and tail work correctly"  ! forAll {(tree: SequenceTree[Int]) =>
    !tree.isEmpty ==> ((tree.head === tree.toLazyList.head) && (tree.tail.toLazyList === tree.toLazyList.tail))
  }

  "last and init work correctly" ! forAll {(tree: SequenceTree[Int]) =>
    !tree.isEmpty ==> ((tree.last === tree.toLazyList.last) && (tree.init.toLazyList === tree.toLazyList.init))
  }

  "foldLeft snoc is identity" ! forAll {
    (tree: SequenceTree[Int]) => tree.foldLeft(FingerTree.empty[Int, Int])(_ :+ _).toLazyList must_==(tree.toLazyList)
  }

  "foldLeft cons is reverse" ! forAll {(tree: SequenceTree[Int]) => tree.foldLeft(FingerTree.empty[Int, Int])((x, y) => y +: x).toLazyList must_==(tree.toLazyList.reverse)}

  "Fingertree" should {

    "apply effects in order" in {
      val s: Writer[String, FingerTree[Int, Int]] = lazyListToTree(intLazyList.take(5)).traverseTree[Writer[String, *], Int, Int](x => Writer(x.toString, x))
      s.run must_===("12345" -> lazyListToTree(intLazyList.take(5)))
    }

    "traverseTree through the option effect yielding result" in {
      val tree = lazyListToTree(intLazyList.take(20)).traverseTree[Option, Int, Int](i => Some(i * 2))
      tree.map(_.toLazyList) getOrElse(LazyList.empty) must_===(lazyListToTree(intLazyList.take(20).map(_ * 2)).toLazyList)
    }

    "traverseTree through the option effect yielding none" in {
      val tree = lazyListToTree(intLazyList.take(20)).traverseTree[Option, Int, Int](i => if (i < 10) Some(i * 2) else None)
      tree must_===(None)
    }

    "not blow the stack" in {
      val tree: Option[FingerTree[Int, Int]] = lazyListToTree(intLazyList.take(32 * 1024)).traverseTree[Option, Int, Int](x => Some(x))
      tree.map(_.toLazyList.take(100)) getOrElse LazyList.empty must_===(intLazyList.take(100))
    }

  }

  "OrdSeq is ordered" ! forAll { (xs: List[Int]) => OrdSeq(xs:_*).toList == xs.sorted }

  "IndSeq" should {
    import org.scalacheck._
    import Arbitrary.arbitrary

    case class TestInstance(arr: Array[Int], index: Int)

    implicit def myGen: Arbitrary[TestInstance] = Arbitrary(for {
      arr <- arbitrary[Array[Int]] if arr.nonEmpty
      m <- Gen.choose(0, arr.length-1)
    } yield TestInstance(arr,m))

    "have a length" ! forAll { (xs: Array[Int]) => IndSeq(xs: _*).length == xs.length }

    "allow random access" ! forAll { (ti: TestInstance) =>
      IndSeq(ti.arr: _*)(ti.index) == ti.arr(ti.index)
    }
  }

  "viewl works correctly" ! forAll {(tree: SequenceTree[Int]) =>
    val asLazyList = tree.toLazyList
    tree.viewl.fold[Boolean](true, (x, t) => (x === asLazyList.head) && (t.toLazyList === asLazyList.tail))
  }

  "viewr works correctly" ! forAll {(tree: SequenceTree[Int]) =>
    val asLazyList = tree.toLazyList
    tree.viewr.fold[Boolean](true, (i, x) => (i.toLazyList ≟ asLazyList.init) && (x ≟ asLazyList.last))
  }
}
