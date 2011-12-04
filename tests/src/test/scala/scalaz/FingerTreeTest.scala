package scalaz

import org.specs2.mutable.Specification
import org.specs2.ScalaCheck
import org.scalacheck.Prop._
import FingerTree._
import scalacheck.ScalazArbitrary._
import std.anyVal._
import syntax.equal._
import std.stream._
import Id._
import WriterT._
import org.scalacheck.{Prop, Arbitrary}
import std.tuple._

class FingerTreeTest extends Specification with ScalaCheck {
  type SequenceTree[A] = FingerTree[Int, A]
  implicit def SizeReducer[A]: Reducer[A, Int] = UnitReducer(x => 1)

  val intStream = Stream.from(1)

  def sampleTree: FingerTree[Int, Int] = asTree(intStream.take(10))

  def asTree[A](stream: Stream[A]): SequenceTree[A] = stream.foldLeft(FingerTree.empty(SizeReducer[A])) {
    case (t, x) => (t :+ x)
  }

  /*Added here to make the tests less awkward to write. It works since we use just one simple reducer for our tests.
   * In general, it seems harder to define equality for fingertrees, since they can be build up using different Reducers, and the
   * question what it means for two trees to be equal becomes harder to answer.
   */
  implicit def fingerTreeEqual[V, A: Equal]: Equal[FingerTree[V, A]] = new Equal[FingerTree[V, A]] {
    def equal(a1: FingerTree[V, A], a2: FingerTree[V, A]) = Equal[Stream[A]].equal(a1.toStream, a2.toStream)
  }

//  override implicit val defaultParameters = Parameters(defaultValues.updated(maxSize, 20))

  "append one element works correctly" ! check {(tree: SequenceTree[Int], x: Int) =>
    (tree :+ x).toStream must be_===(tree.toStream :+ x)
  }

  "prepending one element works correctly" ! check {(tree: SequenceTree[Int], x: Int) =>
    (x +: tree).toStream must be_===(x +: tree.toStream)
  }

  "converting a stream to a finger-tree and back produces an equal stream" ! check {(stream: Stream[Int]) =>
    asTree(stream).toStream must be_===(stream)
  }

  "appending two trees works correctly" ! check {(tree1: SequenceTree[Int], tree2: SequenceTree[Int]) =>
    (tree1 <++> tree2).toStream must be_===(tree1.toStream ++ tree2.toStream)
  }

  "splitting a tree works the same as splitting a stream" ! check {(tree: SequenceTree[Int], index: Int) =>
    val asStream = tree.toStream
    val splitTree = tree.split(_ > index)
    (splitTree._1.toStream, splitTree._2.toStream) must be_===(asStream.splitAt(index))
  }

  "replacing last element works correctly" ! check{(tree: SequenceTree[Int], x: Int) =>
    tree.isEmpty || ((tree :-| x).toStream must be_===(tree.toStream.init :+ x))
  } // can't use conditional property here, it would be better to write !tree.isEmpty ==> ...

  "replacing first element works correctly" ! check {(tree: SequenceTree[Int], x: Int) =>
    tree.isEmpty || ((x |-: tree).toStream must be_=== (x +: tree.toStream.tail))
  }

  "head and tail work correctly"  ! check {(tree: SequenceTree[Int]) =>
    val asStream = tree.toStream
    tree.isEmpty || ((tree.head === asStream.head) && (tree.tail.toStream === asStream.tail))
  }

  "last and init work correctly" ! check {(tree: SequenceTree[Int]) =>
    val asStream = tree.toStream
    tree.isEmpty || ((tree.last === asStream.last) && (tree.init.toStream === asStream.init))
  }

  "foldLeft snoc is identity" ! check {(tree: SequenceTree[Int]) => tree.foldLeft(FingerTree.empty(SizeReducer[Int]))(_ :+ _).toStream ?= tree.toStream}

  "foldLeft cons is reverse" ! check {(tree: SequenceTree[Int]) => tree.foldLeft(FingerTree.empty(SizeReducer[Int]))((x, y) => y +: x).toStream ?= tree.toStream.reverse}

  import std.option._
  import std.list._
  import syntax.applicative._
  import std.string._


  "Fingertree" should {
    "apply effects in order" in {
      val s: Writer[String, FingerTree[Int, Int]] = sampleTree.traverseTree[({type l[a] = Writer[String, a]})#l, Int, Int](x => Writer(x.toString, x))
      //alternatively: type StringWriter[A] = Writer[String, A]; tree.traverseTee[StringWriter, Int, Int]
      val (w, resTree) = s.runT
      w must be_===("12345678910")
      resTree.toStream must be_===(sampleTree toStream)
    }

    "traverseTree through the option effect yielding result" in {
      val tree = sampleTree.traverseTree[Option, Int, Int](i => Some(i * 2))
      tree.map(_ toStream) getOrElse(Stream.empty) must be_===(asTree(intStream.take(10).map(_ * 2)).toStream)
    }

    "traverseTree through the option effect yielding none" in {
      val tree = sampleTree.traverseTree[Option, Int, Int](i => if (i < 10) Some(i * 2) else None)
      tree must be_===(None)
    }

    "not blow the stack" in {
      val tree: Option[FingerTree[Int, Int]] = asTree(intStream.take(32 * 1024)).traverseTree[Option, Int, Int](x => Some(x))
      tree.map(_.toStream.take(10)) getOrElse Stream.empty must be_===(intStream.take(10))
    }

    "identity fingertree traverse" ! check {(tree: SequenceTree[Int]) =>
        identityTraverse[Id, Int, Int, Int](tree)
    }

    "purity fingertree traverse" ! check {(tree: SequenceTree[Int]) =>
      purityLaw[Option, Int, Int](tree)
    }

    "sequential fusion fingertree" ! check {(tree: SequenceTree[Int]) =>
      sequentialFusion[Option, Option, Int, Int, Int, Int](tree) //TODO sequentialfusion for List/Option blows up the heap
    }

    "parallel fusion fingertree" ! check {(tree: SequenceTree[Int]) =>
      parallelFusion[Option, Option, Int, Int, Int](tree)
    }.set(minTestsOk -> 20, maxSize -> 50)
  }

  def identityTraverse[F[_], X, Y, V](tree: FingerTree[V, X])(implicit afx: Arbitrary[F[X]], axy: Arbitrary[X => Y], ef: Equal[FingerTree[V, Y]], ms: Reducer[Y, V]) =
    forAll(identityTraverseLaw[X, Y, V](tree) _)

  def identityTraverseLaw[A, B, V](fa: FingerTree[V, A])(f: A => B)(implicit ms: Reducer[B, V], FB: Equal[FingerTree[V, B]]) = {
    import Id._
    FB.equal(fa.traverseTree[Id, V, B](f), fa.map(f))
  }

  def purityLaw[G[_], A, V](tree: FingerTree[V, A])(implicit G: Applicative[G], ms: Reducer[A, V], GFA: Equal[G[FingerTree[V, A]]]): Boolean = {
    val pointTraverse = tree.traverseTree[G, V, A](G.point(_))
    val pointedTree = G.point(tree)
    GFA.equal(pointTraverse, pointedTree)
  }

  def sequentialFusion[N[_], M[_], A, B, C, V](tree: SequenceTree[A])(implicit amb: Arbitrary[A => M[B]], bnc: Arbitrary[B => N[C]],
                                                    N: Applicative[N], M: Applicative[M], R1: Reducer[B, V], R2: Reducer[C, V], MN: Equal[M[N[FingerTree[V, C]]]]): Prop =
      forAll(sequentialFusionLaw[N, M, A, B, C, V](tree) _)

  /** Two sequentially dependent effects can be fused into one, their composition */
  def sequentialFusionLaw[N[_], M[_], A, B, C, V](fa: SequenceTree[A])(amb: A => M[B], bnc: B => N[C])
                                             (implicit N: Applicative[N], M: Applicative[M], ms1: Reducer[B, V], ms2: Reducer[C, V], MN: Equal[M[N[FingerTree[V, C]]]]): Boolean = {
    type MN[A] = M[N[A]]
    val t1: MN[FingerTree[V, C]] = M.map(fa.traverseTree[M, V, B](amb))(fb => fb.traverseTree[N, V, C](bnc))
    val t2: MN[FingerTree[V, C]] = fa.traverseTree[MN, V, C](a => M.map(amb(a))(b => bnc(b)))(ms2, M compose N)
    MN.equal(t1, t2)
  }

  def parallelFusion[N[_], M[_], A, B, V](tree: SequenceTree[A])(implicit amb: Arbitrary[A => M[B]], anb: Arbitrary[A => N[B]],
                                                N: Applicative[N], M: Applicative[M], R: Reducer[B, V], MN: Equal[(M[FingerTree[V, B]], N[FingerTree[V, B]])]): Prop =
    forAll(parallelFusionLaw[N, M, A, B, V](tree) _)


  /** Two independent effects can be fused into a single effect, their product. */
  def parallelFusionLaw[N[_], M[_], A, B, V](fa: SequenceTree[A])(amb: A => M[B], anb: A => N[B])
                                               (implicit N: Applicative[N], M: Applicative[M], ms: Reducer[B, V], EQ: Equal[(M[FingerTree[V, B]], N[FingerTree[V, B]])]): Boolean = {
      type MN[A] = (M[A], N[A])
      val t1: MN[FingerTree[V, B]] = (fa.traverseTree[M, V, B](amb), fa.traverseTree[N, V, B](anb))
      val t2 = fa.traverseTree[MN, V, B](a => (amb(a), anb(a)))(ms, M product N)
      EQ.equal(t1, t2)
  }

  // TODO reinstate
//  "viewl works correctly" ! check {(tree: SequenceTree[Int]) =>
//    val asStream = tree.toStream
//    tree.viewl.fold[Boolean](true, (x: Int, t: ({type λ[α]=FingerTree[Int, α]})#λ) => (x === asStream.head) && (t.toStream === asStream.tail))
//  }
//
//  "viewr works correctly" verifies {(tree: SequenceTree[Int]) =>
//    val asStream = tree.toStream
//    tree.viewr.fold[Boolean](true, (i: ({type λ[α]=FingerTree[Int, α]})#λ, x: Int) => (i.toStream ≟ asStream.init) && (x ≟ asStream.last))
//  }
}
