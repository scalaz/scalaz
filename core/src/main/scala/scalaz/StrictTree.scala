package scalaz

import scala.collection.mutable
import std.vector.{vectorInstance, vectorMonoid}

/**
  *
  * @param rootLabel The label at the root of this tree.
  * @param subForest The child nodes of this tree.
  * @tparam A
  */
case class StrictTree[A](
  rootLabel: A,
  subForest: Vector[StrictTree[A]]
) {

  import StrictTree._

  /**
    * Run a bottom-up algorithm.
    *
    * This is the framework for several stackless methods, such as map.
    *
    * @param reduce is a function from a label and its mapped children to the new result.
    */
  private[scalaz] def runBottomUp[B](
    reduce: A => mutable.Buffer[B] => B
  ): B = {
    val root = BottomUpStackElem[A, B](None, this)
    val stack = mutable.Stack[BottomUpStackElem[A, B]](root)

    while (stack.nonEmpty) {
      val here = stack.elems.head
      if (here.hasNext) {
        val child = here.next()
        val nextStackElem = BottomUpStackElem[A, B](Some(here), child)
        stack.push(nextStackElem)
      } else {
        //The "here" node is completed, so add its result to its parents completed children.
        val result = reduce(here.rootLabel)(here.mappedSubForest)
        here.parent.foreach(_.mappedSubForest += result)
        stack.pop()
      }
    }

    reduce(root.rootLabel)(root.mappedSubForest)
  }

  /** Maps the elements of the StrictTree into a Monoid and folds the resulting StrictTree. */
  def foldMap[B: Monoid](f: A => B): B =
    runBottomUp(foldMapReducer(f))

  def foldRight[B](z: B)(f: (A, => B) => B): B =
    Foldable[Vector].foldRight(flatten, z)(f)

  /** A 2D String representation of this StrictTree. */
  def drawTree(implicit sh: Show[A]): String = {
    toTree.drawTree
  }

  /** A histomorphic transform. Each element in the resulting tree
    * is a function of the corresponding element in this tree
    * and the histomorphic transform of its children.
    */
  def scanr[B](g: (A, Vector[StrictTree[B]]) => B): StrictTree[B] =
    runBottomUp(scanrReducer(g))

  /** Pre-order traversal. */
  def flatten: Vector[A] = {
    val stack = mutable.Stack(this)

    val result = mutable.Buffer.empty[A]

    while (stack.nonEmpty) {
      val popped = stack.pop()
      result += popped.rootLabel
      popped.subForest.reverseIterator.foreach(stack.push)
    }

    result.toVector
  }

  def size: Int = {
    val stack = mutable.Stack(this.subForest)

    var result = 1

    while (stack.nonEmpty) {
      val popped = stack.pop()
      result += popped.size
      stack.pushAll(popped.map(_.subForest))
    }

    result
  }

  /** Breadth-first traversal. */
  def levels: Vector[Vector[A]] = {
    val f = (s: Vector[StrictTree[A]]) => {
      Foldable[Vector].foldMap(s)((_: StrictTree[A]).subForest)
    }
    Vector.iterate(Vector(this), size)(f) takeWhile (!_.isEmpty) map (_ map (_.rootLabel))
  }

  def toTree: Tree[A] = {
    Tree.Node[A](rootLabel, subForest.toStream.map(_.toTree))
  }

  /** Binds the given function across all the subtrees of this tree. */
  def cobind[B](f: StrictTree[A] => B): StrictTree[B] = unfoldTree(this)(t => (f(t), () => t.subForest))

  def foldNode[Z](f: A => Vector[StrictTree[A]] => Z): Z =
    f(rootLabel)(subForest)

  def map[B](f: A => B): StrictTree[B] = {
    runBottomUp(mapReducer(f))
  }

  def flatMap[B](f: A => StrictTree[B]): StrictTree[B] = {
    runBottomUp(flatMapReducer(f))
  }

  def traverse1[G[_] : Apply, B](f: A => G[B]): G[StrictTree[B]] = {
    val G = Apply[G]

    subForest match {
      case Vector() => G.map(f(rootLabel))(Leaf(_))
      case x +: xs => G.apply2(f(rootLabel), NonEmptyList.nel(x, IList.fromFoldable(xs)).traverse1(_.traverse1(f))) {
        case (h, t) => Node(h, t.list.toVector)
      }
    }
  }

  def zip[B](b: StrictTree[B]): StrictTree[(A, B)] = {
    val root = ZipStackElem[A, B](None, this, b)
    val stack = mutable.Stack[ZipStackElem[A, B]](root)

    while (stack.nonEmpty) {
      val here = stack.elems.head
      if (here.hasNext) {
        val (childA, childB) = here.next()
        val nextStackElem = ZipStackElem[A, B](Some(here), childA, childB)
        stack.push(nextStackElem)
      } else {
        //The "here" node is completed, so add its result to its parents completed children.
        val result = StrictTree((here.a.rootLabel, here.b.rootLabel), here.mappedSubForest.toVector)
        here.parent.foreach(_.mappedSubForest += result)
        stack.pop()
      }
    }

    StrictTree((rootLabel, b.rootLabel), root.mappedSubForest.toVector)
  }

  /**
    * This implementation is 24x faster than the trampolined implementation for StrictTreeTestJVM's hashCode test.
    *
    * @return
    */
  override def hashCode(): Int = {
    runBottomUp(hashCodeReducer)
  }

  override def equals(obj: scala.Any): Boolean = {
    obj match {
      case other: StrictTree[A] =>
        StrictTree.badEqInstance[A].equal(this, other)
      case _ =>
        false
    }
  }
}

sealed abstract class StrictTreeInstances {
  implicit val strictTreeInstance: Traverse1[StrictTree] with Monad[StrictTree] with Comonad[StrictTree] with Align[StrictTree] with Zip[StrictTree] = new Traverse1[StrictTree] with Monad[StrictTree] with Comonad[StrictTree] with Align[StrictTree] with Zip[StrictTree] {
    def point[A](a: => A): StrictTree[A] = StrictTree.Leaf(a)
    def cobind[A, B](fa: StrictTree[A])(f: StrictTree[A] => B): StrictTree[B] = fa cobind f
    def copoint[A](p: StrictTree[A]): A = p.rootLabel
    override def map[A, B](fa: StrictTree[A])(f: A => B) = fa map f
    def bind[A, B](fa: StrictTree[A])(f: A => StrictTree[B]): StrictTree[B] = fa flatMap f
    def traverse1Impl[G[_]: Apply, A, B](fa: StrictTree[A])(f: A => G[B]): G[StrictTree[B]] = fa traverse1 f
    override def foldRight[A, B](fa: StrictTree[A], z: => B)(f: (A, => B) => B): B = fa.foldRight(z)(f)
    override def foldMapRight1[A, B](fa: StrictTree[A])(z: A => B)(f: (A, => B) => B) = (fa.flatten.reverse: @unchecked) match {
      case h +: t => t.foldLeft(z(h))((b, a) => f(a, b))
    }
    override def foldLeft[A, B](fa: StrictTree[A], z: B)(f: (B, A) => B): B =
      fa.flatten.foldLeft(z)(f)
    override def foldMapLeft1[A, B](fa: StrictTree[A])(z: A => B)(f: (B, A) => B): B = fa.flatten match {
      case h +: t => t.foldLeft(z(h))(f)
    }
    override def foldMap[A, B](fa: StrictTree[A])(f: A => B)(implicit F: Monoid[B]): B = fa foldMap f

    //This implementation is 14x faster than the trampolined implementation for StrictTreeTestJVM's align test.
    override def alignWith[A, B, C](f: (\&/[A, B]) => C): (StrictTree[A], StrictTree[B]) => StrictTree[C] = {
      (a, b) =>
        import StrictTree.AlignStackElem
        val root = AlignStackElem[A, B, C](None, \&/(a, b))
        val stack = mutable.Stack(root)

        while (stack.nonEmpty) {
          val here = stack.elems.head
          if (here.hasNext) {
            val nextChildren = here.next()
            val nextStackElem = AlignStackElem[A, B, C](Some(here), nextChildren)
            stack.push(nextStackElem)
          } else {
            //The "here" node is completed, so add its result to its parents completed children.
            val result = StrictTree[C](f(here.trees.bimap(_.rootLabel, _.rootLabel)), here.mappedSubForest.toVector)
            here.parent.foreach(_.mappedSubForest += result)
            stack.pop()
          }
        }

        StrictTree(f(root.trees.bimap(_.rootLabel, _.rootLabel)), root.mappedSubForest.toVector)
    }

    override def zip[A, B](a: => StrictTree[A], b: => StrictTree[B]): StrictTree[(A, B)] = {
      a.zip(b)
    }
  }

  implicit def treeEqual[A](implicit A0: Equal[A]): Equal[StrictTree[A]] =
    new StrictTreeEqual[A] { def A = A0 }

  implicit def treeOrder[A](implicit A0: Order[A]): Order[StrictTree[A]] =
    new Order[StrictTree[A]] with StrictTreeEqual[A] {
      def A = A0
      import std.vector._
      override def order(x: StrictTree[A], y: StrictTree[A]) =
        A.order(x.rootLabel, y.rootLabel) match {
          case Ordering.EQ =>
            Order[Vector[StrictTree[A]]].order(x.subForest, y.subForest)
          case x => x
        }
    }



  /* TODO
  def applic[A, B](f: StrictTree[A => B]) = a => StrictTree.node((f.rootLabel)(a.rootLabel), implicitly[Applic[newtypes.ZipVector]].applic(f.subForest.map(applic[A, B](_)).?)(a.subForest ?).value)
   */
}


object StrictTree extends StrictTreeInstances {
  /**
   * Node represents a tree node that may have children.
   *
   * You can use Node for tree construction or pattern matching.
   */
  object Node {
    def apply[A](root: A, forest: Vector[StrictTree[A]]): StrictTree[A] = {
      StrictTree[A](root, forest)
    }

    def unapply[A](t: StrictTree[A]): Option[(A, Vector[StrictTree[A]])] = Some((t.rootLabel, t.subForest))
  }

  /**
   *  Leaf represents a a tree node with no children.
   *
   *  You can use Leaf for tree construction or pattern matching.
   */
  object Leaf {
    def apply[A](root: A): StrictTree[A] = {
      Node(root, Vector.empty)
    }

    def unapply[A](t: StrictTree[A]): Option[A] = {
      t match {
        case Node(root, Vector()) =>
          Some(root)
        case _ =>
          None
      }
    }
  }

  def unfoldForest[A, B](s: Vector[A])(f: A => (B, () => Vector[A])): Vector[StrictTree[B]] =
    s.map(unfoldTree(_)(f))

  def unfoldTree[A, B](v: A)(f: A => (B, () => Vector[A])): StrictTree[B] =
    f(v) match {
      case (a, bs) => Node(a, unfoldForest(bs.apply())(f))
    }

  //Only used for .equals.
  private def badEqInstance[A] = new StrictTreeEqual[A] {
    override def A: Equal[A] = new Equal[A] {
      override def equal(a1: A, a2: A): Boolean = a1.equals(a2)
    }
  }

  /**
    * This implementation is 16x faster than the trampolined implementation for StrictTreeTestJVM's scanr test.
    */
  private def scanrReducer[A, B](
    f: (A, Vector[StrictTree[B]]) => B
  )(rootLabel: A
  )(subForest: mutable.Buffer[StrictTree[B]]
  ): StrictTree[B] = {
    val subForestVector = subForest.toVector
    StrictTree[B](f(rootLabel, subForestVector), subForestVector)
  }

  /**
    * This implementation is 10x faster than mapTrampoline for StrictTreeTestJVM's map test.
    */
  private def mapReducer[A, B](
    f: A => B
  )(rootLabel: A
  )(subForest: Seq[StrictTree[B]]
  ): StrictTree[B] = {
    StrictTree[B](f(rootLabel), subForest.toVector)
  }

  /**
    * This implementation is 9x faster than flatMapTrampoline for StrictTreeTestJVM's flatMap test.
    */
  private def flatMapReducer[A, B](
    f: A => StrictTree[B]
  )(root: A
  )(subForest: Seq[StrictTree[B]]
  ): StrictTree[B] = {
    val StrictTree(rootLabel0, subForest0) = f(root)
    StrictTree(rootLabel0, subForest0 ++ subForest)
  }

  /**
    * This implementation is 9x faster than the trampolined implementation for StrictTreeTestJVM's foldMap test.
    */
  private def foldMapReducer[A, B: Monoid](
    f: A => B
  )(rootLabel: A
  )(subForest: mutable.Buffer[B]
  ): B = {
    val mappedRoot = f(rootLabel)
    val foldedForest = Foldable[Vector].fold[B](subForest.toVector)

    Monoid[B].append(mappedRoot, foldedForest)
  }

  private def hashCodeReducer[A](root: A)(subForest: Seq[Int]): Int = {
    root.hashCode ^ subForest.hashCode
  }

  private case class BottomUpStackElem[A, B](
    parent: Option[BottomUpStackElem[A, B]],
    tree: StrictTree[A]
  ) extends Iterator[StrictTree[A]] {
    private val subIterator = tree.subForest.iterator

    def rootLabel = tree.rootLabel

    val mappedSubForest: mutable.Buffer[B] = mutable.Buffer.empty

    override def hasNext: Boolean = subIterator.hasNext

    override def next(): StrictTree[A] = subIterator.next()
  }

  private case class ZipStackElem[A, B](
    parent: Option[ZipStackElem[A, B]],
    a: StrictTree[A],
    b: StrictTree[B]
  ) extends Iterator[(StrictTree[A], StrictTree[B])] {
    private val zippedSubIterator =
      a.subForest.iterator.zip(b.subForest.iterator)

    val mappedSubForest: mutable.Buffer[StrictTree[(A, B)]] = mutable.Buffer.empty

    override def hasNext: Boolean = zippedSubIterator.hasNext

    override def next(): (StrictTree[A], StrictTree[B]) = zippedSubIterator.next()
  }

  private[scalaz] case class AlignStackElem[A, B, C](
    parent: Option[AlignStackElem[A, B, C]],
    trees: \&/[StrictTree[A], StrictTree[B]]
  ) extends Iterator[\&/[StrictTree[A], StrictTree[B]]] {
    private val iterators =
      trees.bimap(_.subForest.iterator, _.subForest.iterator)

    val mappedSubForest: mutable.Buffer[StrictTree[C]] = mutable.Buffer.empty

    def whichHasNext: \&/[Boolean, Boolean] =
      iterators.bimap(_.hasNext, _.hasNext)

    override def hasNext: Boolean =
      whichHasNext.fold(identity, identity, _ || _)

    override def next(): \&/[StrictTree[A], StrictTree[B]] =
      whichHasNext match {
        case \&/(true, true) =>
          iterators.bimap(_.next(), _.next())

        case \&/(true, false) | \&/.This(true) =>
          \&/.This(iterators.onlyThis.get.next())

        case \&/(false, true) | \&/.That(true) =>
          \&/.That(iterators.onlyThat.get.next())

        case _ =>
          throw new NoSuchElementException("reached iterator end")
      }
  }

  implicit def ToStrictTreeUnzip[A1, A2](root: StrictTree[(A1, A2)]): StrictTreeUnzip[A1, A2] =
    new StrictTreeUnzip[A1, A2](root)

}

private trait StrictTreeEqual[A] extends Equal[StrictTree[A]] {
  def A: Equal[A]

  private case class EqualStackElem(
    a: StrictTree[A],
    b: StrictTree[A]
  ) {
    val aSubIterator =
      a.subForest.iterator

    val bSubIterator =
      b.subForest.iterator
  }

  //This implementation is 4.5x faster than the trampolined implementation for StrictTreeTestJVM's equal test.
  override final def equal(a1: StrictTree[A], a2: StrictTree[A]): Boolean = {
    val root = EqualStackElem(a1, a2)
    val stack = mutable.Stack[EqualStackElem](root)

    while (stack.nonEmpty) {
      val here = stack.elems.head
      if (A.equal(here.a.rootLabel, here.b.rootLabel)) {
        val aNext = here.aSubIterator.hasNext
        val bNext = here.bSubIterator.hasNext
        (aNext, bNext) match {
          case (true, true) =>
            val childA = here.aSubIterator.next()
            val childB = here.bSubIterator.next()
            val nextStackElem = EqualStackElem(childA, childB)
            stack.push(nextStackElem)
          case (false, false) =>
            stack.pop()
          case _ =>
            return false
        }
      } else return false
    }

    true
  }
}

final class StrictTreeUnzip[A1, A2](val root: StrictTree[(A1, A2)]) extends AnyVal {
  private def unzipCombiner(rootLabel: (A1, A2))(accumulator: Seq[(StrictTree[A1], StrictTree[A2])]): (StrictTree[A1], StrictTree[A2]) = {
    (StrictTree(rootLabel._1, accumulator.map(_._1).toVector), StrictTree(rootLabel._2, accumulator.map(_._2).toVector))
  }

  /** Turns a tree of pairs into a pair of trees. */
  def unzip: (StrictTree[A1], StrictTree[A2]) = {
    root.runBottomUp[(StrictTree[A1], StrictTree[A2])](unzipCombiner)
  }
}
