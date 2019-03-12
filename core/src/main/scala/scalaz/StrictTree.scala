package scalaz

import scala.collection.mutable
import scala.util.hashing.MurmurHash3

/**
  *
  * @param rootLabel The label at the root of this tree.
  * @param subForest The child nodes of this tree.
  * @tparam A
  */
case class StrictTree[A](
  rootLabel: A,
  subForest: List[StrictTree[A]]
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
    reduce: A => mutable.ListBuffer[B] => B
  ): B = {
    val root = BottomUpStackElem[A, B](None, this)
    var stack = root :: Nil

    while (stack.nonEmpty) {
      val here = stack.head
      if (here.hasNext) {
        val child = here.next()
        val nextStackElem = BottomUpStackElem[A, B](Some(here), child)
        stack = nextStackElem :: stack
      } else {
        //The "here" node is completed, so add its result to its parents completed children.
        val result = reduce(here.rootLabel)(here.mappedSubForest)
        here.parent.foreach(_.mappedSubForest += result)
        stack = stack.tail
      }
    }

    reduce(root.rootLabel)(root.mappedSubForest)
  }

  /** Maps the elements of the StrictTree into a Monoid and folds the resulting StrictTree. */
  def foldMap[B: Monoid](f: A => B): B =
    foldLeft(Monoid[B].zero)((a, b) => Monoid[B].append(b, f(a)))

  def foldLeft[B](z: B)(f: (A, B) => B): B = {
    var stack = List(this) :: Nil
    var result = z
    while (stack.nonEmpty) {
      val head :: tail = stack
      if (head.isEmpty) {
        stack = tail
      } else {
        val h2 :: t2 = head
        result = f(h2.rootLabel, result)
        stack = h2.subForest :: t2 :: tail
      }
    }
    result
  }

  def foldRight[B](z: B)(f: (A, => B) => B): B =
    rflatten.foldLeft(z)((a, b) => f(b, a))

  /** A 2D String representation of this StrictTree. */
  def drawTree(implicit sh: Show[A]): String = {
    toTree.drawTree
  }

  /** A histomorphic transform. Each element in the resulting tree
    * is a function of the corresponding element in this tree
    * and the histomorphic transform of its children.
    */
  def scanr[B](g: (A, List[StrictTree[B]]) => B): StrictTree[B] =
    runBottomUp(scanrReducer(g))

  /** Pre-order traversal. */
  def flatten: List[A] = rflatten.reverse

  /** Reverse pre-order traversal. */
  def rflatten: List[A] = foldLeft(List.empty[A])(_ :: _)

  def size: Int = foldLeft(0)((_, b) => b + 1)

  /** Breadth-first traversal. */
  def levels: List[List[A]] = {
    var level = List(this)

    val result = mutable.ListBuffer.empty[List[A]]

    while (level.nonEmpty) {
      result += level.map(_.rootLabel)
      level = level.flatMap(_.subForest)
    }

    result.toList
  }

  def toTree: Tree[A] = {
    Tree.Node[A](rootLabel, subForest.toStream.map(_.toTree))
  }

  /** Binds the given function across all the subtrees of this tree. */
  def cobind[B](f: StrictTree[A] => B): StrictTree[B] = unfoldTree(this)(t => (f(t), t.subForest))

  def foldNode[Z](f: A => List[StrictTree[A]] => Z): Z =
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
      case Nil => G.map(f(rootLabel))(Leaf(_))
      case x :: xs => G.apply2(f(rootLabel), NonEmptyList.nel(x, IList.fromList(xs)).traverse1(_.traverse1(f))) {
        case (h, t) => Node(h, t.list.toList)
      }
    }
  }

  def zip[B](b: StrictTree[B]): StrictTree[(A, B)] = {
    val root = ZipStackElem[A, B](None, this, b)
    var stack = root :: Nil

    while (stack.nonEmpty) {
      val here = stack.head
      if (here.hasNext) {
        val (childA, childB) = here.next()
        val nextStackElem = ZipStackElem[A, B](Some(here), childA, childB)
        stack = nextStackElem :: stack
      } else {
        //The "here" node is completed, so add its result to its parents completed children.
        val result = StrictTree((here.a.rootLabel, here.b.rootLabel), here.mappedSubForest.toList)
        here.parent.foreach(_.mappedSubForest += result)
        stack = stack.tail
      }
    }

    StrictTree((rootLabel, b.rootLabel), root.mappedSubForest.toList)
  }

  /**
    * This implementation is 24x faster than the trampolined implementation for StrictTreeTestJVM's hashCode test.
    *
    * @return
    */
  override def hashCode(): Int =
    MurmurHash3.listHash(rflatten, "StrictTree".hashCode)

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
    override def alignWith[A, B, C](f: A \&/ B => C): (StrictTree[A], StrictTree[B]) => StrictTree[C] = {
      (a, b) =>
        import StrictTree.AlignStackElem
        val root = AlignStackElem[A, B, C](None, \&/(a, b))
        var stack = root :: Nil

        while (stack.nonEmpty) {
          val here = stack.head
          if (here.hasNext) {
            val nextChildren = here.next()
            val nextStackElem = AlignStackElem[A, B, C](Some(here), nextChildren)
            stack = nextStackElem :: stack
          } else {
            //The "here" node is completed, so add its result to its parents completed children.
            val result = StrictTree[C](f(here.trees.bimap(_.rootLabel, _.rootLabel)), here.mappedSubForest.toList)
            here.parent.foreach(_.mappedSubForest += result)
            stack = stack.tail
          }
        }

        StrictTree(f(root.trees.bimap(_.rootLabel, _.rootLabel)), root.mappedSubForest.toList)
    }

    override def zip[A, B](a: => StrictTree[A], b: => StrictTree[B]): StrictTree[(A, B)] = {
      a.zip(b)
    }
  }

  implicit def treeEqual[A](implicit A0: Equal[A]): Equal[StrictTree[A]] =
    new StrictTreeEqual[A] { def A = A0 }

  implicit def treeOrder[A](implicit A0: Order[A]): Order[StrictTree[A]] =
    new Order[StrictTree[A]] with StrictTreeEqual[A] {
      import std.list.listOrder
      def A = A0
      override def order(x: StrictTree[A], y: StrictTree[A]) =
        A.order(x.rootLabel, y.rootLabel) match {
          case Ordering.EQ =>
            Order[List[StrictTree[A]]].order(x.subForest, y.subForest)
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
    def apply[A](root: A, forest: List[StrictTree[A]]): StrictTree[A] = {
      StrictTree[A](root, forest)
    }

    def unapply[A](t: StrictTree[A]): Option[(A, List[StrictTree[A]])] = Some((t.rootLabel, t.subForest))
  }

  /**
   *  Leaf represents a tree node with no children.
   *
   *  You can use Leaf for tree construction or pattern matching.
   */
  object Leaf {
    def apply[A](root: A): StrictTree[A] = {
      Node(root, Nil)
    }

    def unapply[A](t: StrictTree[A]): Option[A] = {
      t match {
        case Node(root, List()) =>
          Some(root)
        case _ =>
          None
      }
    }
  }

  def unfoldForest[A, B](s: List[A])(f: A => (B, List[A])): List[StrictTree[B]] =
    s.map(unfoldTree(_)(f))

  def unfoldTree[A, B](v: A)(f: A => (B, List[A])): StrictTree[B] =
    f(v) match {
      case (a, bs) => Node(a, unfoldForest(bs)(f))
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
    f: (A, List[StrictTree[B]]) => B
  )(rootLabel: A
  )(subForest: mutable.ListBuffer[StrictTree[B]]
  ): StrictTree[B] = {
    val subForestList = subForest.toList
    StrictTree[B](f(rootLabel, subForestList), subForestList)
  }

  /**
    * This implementation is 10x faster than mapTrampoline for StrictTreeTestJVM's map test.
    */
  private def mapReducer[A, B](
    f: A => B
  )(rootLabel: A
  )(subForest: mutable.ListBuffer[StrictTree[B]]
  ): StrictTree[B] = {
    StrictTree[B](f(rootLabel), subForest.toList)
  }

  /**
    * This implementation is 9x faster than flatMapTrampoline for StrictTreeTestJVM's flatMap test.
    */
  private def flatMapReducer[A, B](
    f: A => StrictTree[B]
  )(root: A
  )(subForest: mutable.ListBuffer[StrictTree[B]]
  ): StrictTree[B] = {
    val StrictTree(rootLabel0, subForest0) = f(root)
    StrictTree(rootLabel0, subForest0 ++ subForest)
  }

  private final case class BottomUpStackElem[A, B](
    parent: Option[BottomUpStackElem[A, B]],
    tree: StrictTree[A]
  ) extends Iterator[StrictTree[A]] {
    private[this] var subPosition = tree.subForest

    private[scalaz] def rootLabel = tree.rootLabel

    private[scalaz] val mappedSubForest = mutable.ListBuffer.empty[B]

    override def hasNext: Boolean = subPosition.nonEmpty

    override def next(): StrictTree[A] = {
      val head = subPosition.head
      subPosition = subPosition.tail
      head
    }
  }

  private final case class ZipStackElem[A, B](
    parent: Option[ZipStackElem[A, B]],
    a: StrictTree[A],
    b: StrictTree[B]
  ) extends Iterator[(StrictTree[A], StrictTree[B])] {
    private[this] var subPosition = STreeZip(a.subForest, b.subForest)

    private[scalaz] val mappedSubForest = mutable.ListBuffer.empty[StrictTree[(A, B)]]

    override def hasNext: Boolean = subPosition.as.nonEmpty && subPosition.bs.nonEmpty

    override def next(): (StrictTree[A], StrictTree[B]) = {
      val head = (subPosition.as.head, subPosition.bs.head)
      subPosition = STreeZip(subPosition.as.tail, subPosition.bs.tail)
      head
    }
  }

  private[scalaz] final case class AlignStackElem[A, B, C](
    parent: Option[AlignStackElem[A, B, C]],
    trees: \&/[StrictTree[A], StrictTree[B]]
  ) extends Iterator[\&/[StrictTree[A], StrictTree[B]]] {
    private[this] var subPosition = STreeZip(
      trees.a.map(_.subForest).getOrElse(List.empty),
      trees.b.map(_.subForest).getOrElse(List.empty)
    )

    private[scalaz] val mappedSubForest = mutable.ListBuffer.empty[StrictTree[C]]

    override def hasNext: Boolean = subPosition.as.nonEmpty || subPosition.bs.nonEmpty

    override def next(): \&/[StrictTree[A], StrictTree[B]] =
      subPosition match {
        case STreeZip(a :: aTail, b :: bTail) =>
          subPosition = STreeZip(aTail, bTail)
          \&/.Both(a, b)
        case STreeZip(a :: aTail, Nil) =>
          subPosition = STreeZip(aTail, Nil)
          \&/.This(a)
        case STreeZip(Nil, b :: bTail) =>
          subPosition = STreeZip(Nil, bTail)
          \&/.That(b)
        case STreeZip(Nil, Nil) =>
          throw new NoSuchElementException("reached iterator end")
      }
  }

  implicit def ToStrictTreeUnzip[A1, A2](root: StrictTree[(A1, A2)]): StrictTreeUnzip[A1, A2] =
    new StrictTreeUnzip[A1, A2](root)

}

private trait StrictTreeEqual[A] extends Equal[StrictTree[A]] {
  def A: Equal[A]

  //This implementation is 4.5x faster than the trampolined implementation for StrictTreeTestJVM's equal test.
  override final def equal(a1: StrictTree[A], a2: StrictTree[A]): Boolean = {
    import StrictTree.Node

    if (!A.equal(a1.rootLabel, a2.rootLabel))
      return false

    var stack = STreeZip(a1.subForest, a2.subForest) :: Nil

    while (stack.nonEmpty) {
      stack match {
        case STreeZip(Node(childA1, childrenA1) :: a1Tail, Node(childA2, childrenA2) :: a2Tail) :: tail=>
          if (!A.equal(childA1, childA2))
            return false
          stack = STreeZip(a1Tail, a2Tail) :: STreeZip(childrenA1, childrenA2) :: tail
        case STreeZip(Nil, Nil) :: tail =>
          stack = tail
        case _ =>
          return false
      }
    }

    true
  }
}

final class StrictTreeUnzip[A1, A2](private val root: StrictTree[(A1, A2)]) extends AnyVal {
  private def unzipCombiner(rootLabel: (A1, A2))(accumulator: mutable.ListBuffer[(StrictTree[A1], StrictTree[A2])]): (StrictTree[A1], StrictTree[A2]) = {
    (StrictTree(rootLabel._1, accumulator.map(_._1).toList), StrictTree(rootLabel._2, accumulator.map(_._2).toList))
  }

  /** Turns a tree of pairs into a pair of trees. */
  def unzip: (StrictTree[A1], StrictTree[A2]) = {
    root.runBottomUp[(StrictTree[A1], StrictTree[A2])](unzipCombiner)
  }
}

private[scalaz] final case class STreeZip[A, B](
  as: List[StrictTree[A]],
  bs: List[StrictTree[B]]
)
