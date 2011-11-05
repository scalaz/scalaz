package scalaz

import Scalaz._

/** A rose-tree zipper. Represents a scalaz.Tree together with a position in that tree. Provides navigation, persistent update, insertion, and deletes. */
sealed trait TreeLoc[+A] {

  /** The currently selected node. */
  val tree: Tree[A]

  /** The left siblings of the current node. */
  val lefts: Stream[Tree[A]]

  /** The right siblings of the current node. */ 
  val rights: Stream[Tree[A]]

  /** The parent contexts of the current node. */
  val parents: Stream[(Stream[Tree[A]], A, Stream[Tree[A]])]

  /** Select the parent of the current node. */
  def parent: Option[TreeLoc[A]] = parents match {
    case (pls, v, prs) #:: ps => Some(loc(node(v, combChildren(lefts, tree, rights)), pls, prs, ps))
    case Stream.Empty => None
  }

  /** Select the root node of the tree. */
  def root: TreeLoc[A] = parent.some(_.root).none(this)

  /** Select the left sibling of the current node. */
  def left: Option[TreeLoc[A]] = lefts match {
    case t #:: ts => Some(loc(t, ts, tree #:: rights, parents))
    case Stream.Empty => None
  }

  /** Select the right sibling of the current node. */
  def right: Option[TreeLoc[A]] = rights match {
    case t #:: ts => Some(loc(t, tree #:: lefts, ts, parents))
    case Stream.Empty => None
  }

  /** Select the leftmost child of the current node. */
  def firstChild: Option[TreeLoc[A]] = tree.subForest match {
    case t #:: ts => Some(loc(t, Stream.Empty, ts, downParents))
    case Stream.Empty => None
  }

  /** Select the rightmost child of the current node. */
  def lastChild: Option[TreeLoc[A]] = tree.subForest.reverse match {
    case t #:: ts => Some(loc(t, ts, Stream.Empty, downParents))
    case Stream.Empty => None
  }

  /** Select the nth child of the current node. */
  def getChild(n: Int): Option[TreeLoc[A]] =
    for{lr <- splitChildren(Stream.Empty, tree.subForest, n)
        ls = lr._1
    } yield loc(ls.head, ls.tail, lr._2, downParents)

  /** Select the first immediate child of the current node that satisfies the given predicate. */
  def findChild(p: Tree[A] => Boolean): Option[TreeLoc[A]] = {
    def split(acc: Stream[Tree[A]], xs: Stream[Tree[A]]): Option[(Stream[Tree[A]], Tree[A], Stream[Tree[A]])] =
      (acc, xs) match {
        case (acc, Stream.cons(x, xs)) => if (p(x)) Some((acc, x, xs)) else split(Stream.cons(x, acc), xs)
        case _ => None
      }
    for (ltr <- split(Stream.Empty, tree.subForest)) yield loc(ltr._2, ltr._1, ltr._3, downParents)
  }

  /** Select the first descendant node of the current node that satisfies the given predicate. */
  def find(p: TreeLoc[A] => Boolean): Option[TreeLoc[A]] =
    this.cojoin.tree.flatten.find(p)

  /** Get the entire tree represented by this zipper. */
  def toTree: Tree[A] = root.tree

  /** Get the entire forest represented by this zipper. */
  def toForest: Stream[Tree[A]] = combChildren(root.lefts, root.tree, root.rights)

  /** True if the current node is the root node. */
  def isRoot: Boolean = parents.isEmpty

  /** True if the current node has no left siblings.  */
  def isFirst: Boolean = lefts.isEmpty

  /** True if the current node has no right siblings. */
  def isLast: Boolean = rights.isEmpty

  /** True if the current node has no children.  */
  def isLeaf: Boolean = tree.subForest.isEmpty

  /** True if the current node is not the root node. */
  def isChild: Boolean = !isRoot

  /** True if the current node has children. */
  def hasChildren: Boolean = !isLeaf

  /** Replace the current node with the given one. */
  def setTree[AA >: A](t: Tree[AA]): TreeLoc[AA] = loc(t, lefts, rights, parents)

  /** Modify the current node with the given function. */
  def modifyTree[AA >: A](f: Tree[AA] => Tree[AA]): TreeLoc[AA] = setTree(f(tree))

  /** Modify the label at the current node with the given function. */
  def modifyLabel[AA >: A](f: AA => AA): TreeLoc[AA] = setLabel(f(getLabel))

  /** Get the label of the current node. */
  def getLabel: A = tree.rootLabel

  /** Set the label of the current node. */
  def setLabel[AA >: A](a: AA): TreeLoc[AA] = modifyTree((t: Tree[AA]) => node(a, t.subForest))

  /** Insert the given node to the left of the current node and give it focus. */
  def insertLeft[AA >: A](t: Tree[AA]): TreeLoc[AA] = loc(t, lefts, Stream.cons(tree, rights), parents)

  /** Insert the given node to the right of the current node and give it focus. */
  def insertRight[AA >: A](t: Tree[AA]): TreeLoc[AA] = loc(t, Stream.cons(tree, lefts), rights, parents)

  /** Insert the given node as the first child of the current node and give it focus. */
  def insertDownFirst[AA >: A](t: Tree[AA]): TreeLoc[AA] = loc(t, Stream.Empty, tree.subForest, downParents)

  /** Insert the given node as the last child of the current node and give it focus. */
  def insertDownLast[AA >: A](t: Tree[AA]): TreeLoc[AA] = loc(t, tree.subForest.reverse, Stream.Empty, downParents)

  /** Insert the given node as the nth child of the current node and give it focus. */
  def insertDownAt[AA >: A](n: Int, t: Tree[AA]): Option[TreeLoc[AA]] =
    for (lr <- splitChildren(Stream.Empty, tree.subForest, n)) yield loc(t, lr._1, lr._2, downParents)

  /** Delete the current node and all its children. */
  def delete: Option[TreeLoc[A]] = rights match {
    case Stream.cons(t, ts) => Some(loc(t, lefts, ts, parents))
    case _ => lefts match {
      case Stream.cons(t, ts) => Some(loc(t, ts, rights, parents))
      case _ => for (loc1 <- parent) yield loc1.modifyTree((t: Tree[A]) => node(t.rootLabel, Stream.Empty))
    }
  }

  /**
   * The path from the focus to the root.
   */
  def path: Stream[A] = getLabel #:: parents.map(_._2)

  private def downParents = (lefts, tree.rootLabel, rights) #:: parents

  private def combChildren[A](ls: Stream[A], t: A, rs: Stream[A]) =
    ls.foldl(t #:: rs)((a, b) => b #:: a)

  private def splitChildren[A](acc: Stream[A], xs: Stream[A], n: Int): Option[(Stream[A], Stream[A])] =
    (acc, xs, n) match {
      case (acc, xs, 0) => Some((acc, xs))
      case (acc, Stream.cons(x, xs), n) => splitChildren(Stream.cons(x, acc), xs, n - 1)
      case _ => None
    }  
}

trait TreeLocs {
  def loc[A](t: Tree[A], l: Stream[Tree[A]], r: Stream[Tree[A]], p: Stream[(Stream[Tree[A]], A, Stream[Tree[A]])]): TreeLoc[A] =
    new TreeLoc[A] {
      val tree = t
      val lefts = l
      val rights = r
      val parents = p

      override def toString = "<treeloc>"
    }

  def fromForest[A](ts: Stream[Tree[A]]) = ts match {
    case (Stream.cons(t, ts)) => Some(loc(t, Stream.Empty, ts, Stream.Empty))
  }
}
