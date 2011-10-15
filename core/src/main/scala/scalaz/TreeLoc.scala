package scalaz

sealed trait TreeLoc[A] {

  import TreeLoc._
  import Tree._

  val tree: Tree[A]
  val lefts: TreeForest[A]
  val rights: TreeForest[A]
  val parents: Parents[A]

  def parent: Option[TreeLoc[A]] = parents match {
    case (pls, v, prs) #:: ps => Some(loc(node(v, combChildren(lefts, tree, rights)), pls, prs, ps))
    case Stream.Empty => None
  }

  def root: TreeLoc[A] =
    parent match {
      case Some(z) => z.root
      case None => this
    }

  def left: Option[TreeLoc[A]] = lefts match {
    case t #:: ts => Some(loc(t, ts, tree #:: rights, parents))
    case Stream.Empty => None
  }

  def right: Option[TreeLoc[A]] = rights match {
    case t #:: ts => Some(loc(t, tree #:: lefts, ts, parents))
    case Stream.Empty => None
  }

  def firstChild: Option[TreeLoc[A]] = tree.subForest match {
    case t #:: ts => Some(loc(t, Stream.Empty, ts, downParents))
    case Stream.Empty => None
  }

  def lastChild: Option[TreeLoc[A]] = tree.subForest.reverse match {
    case t #:: ts => Some(loc(t, ts, Stream.Empty, downParents))
    case Stream.Empty => None
  }

  def getChild(n: Int): Option[TreeLoc[A]] =
    for {lr <- splitChildren(Stream.Empty, tree.subForest, n)
         ls = lr._1
    } yield loc(ls.head, ls.tail, lr._2, downParents)

  def findChild(p: Tree[A] => Boolean): Option[TreeLoc[A]] = {
    def split(acc: TreeForest[A], xs: TreeForest[A]): Option[(TreeForest[A], Tree[A], TreeForest[A])] =
      (acc, xs) match {
        case (acc, Stream.cons(x, xs)) => if (p(x)) Some((acc, x, xs)) else split(Stream.cons(x, acc), xs)
        case _ => None
      }
    for (ltr <- split(Stream.Empty, tree.subForest)) yield loc(ltr._2, ltr._1, ltr._3, downParents)
  }

  def toTree: Tree[A] = root.tree

  def toForest: TreeForest[A] = combChildren(root.lefts, root.tree, root.rights)

  def isRoot: Boolean = parents.isEmpty

  def isFirst: Boolean = lefts.isEmpty

  def isLast: Boolean = rights.isEmpty

  def isLeaf: Boolean = tree.subForest.isEmpty

  def isChild: Boolean = !isRoot

  def hasChildren: Boolean = !isLeaf

  def setTree(t: Tree[A]): TreeLoc[A] = loc(t, lefts, rights, parents)

  def modifyTree(f: Tree[A] => Tree[A]): TreeLoc[A] = setTree(f(tree))

  def modifyLabel(f: A => A): TreeLoc[A] = setLabel(f(getLabel))

  def getLabel: A = tree.rootLabel

  def setLabel(a: A): TreeLoc[A] = modifyTree((t: Tree[A]) => node(a, t.subForest))

  def insertLeft(t: Tree[A]): TreeLoc[A] = loc(t, lefts, Stream.cons(tree, rights), parents)

  def insertRight(t: Tree[A]): TreeLoc[A] = loc(t, Stream.cons(tree, lefts), rights, parents)

  def insertDownFirst(t: Tree[A]): TreeLoc[A] = loc(t, Stream.Empty, tree.subForest, downParents)

  def insertDownLast(t: Tree[A]): TreeLoc[A] = loc(t, tree.subForest.reverse, Stream.Empty, downParents)

  def insertDownAt(n: Int, t: Tree[A]): Option[TreeLoc[A]] =
    for (lr <- splitChildren(Stream.Empty, tree.subForest, n)) yield loc(t, lr._1, lr._2, downParents)

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
    ls.foldLeft(t #:: rs)((a, b) => b #:: a)

  private def splitChildren[A](acc: Stream[A], xs: Stream[A], n: Int): Option[(Stream[A], Stream[A])] =
    (acc, xs, n) match {
      case (acc, xs, 0) => Some((acc, xs))
      case (acc, Stream.cons(x, xs), n) => splitChildren(Stream.cons(x, acc), xs, n - 1)
      case _ => None
    }
}

object TreeLoc extends TreeLocs {
  def apply[A](t: Tree[A], l: TreeForest[A], r: TreeForest[A], p: Parents[A]): TreeLoc[A] =
    loc(t, l, r, p)
}

trait TreeLocs {
  type TreeForest[A] =
  Stream[Tree[A]]

  type Parent[A] =
  (TreeForest[A], A, TreeForest[A])

  type Parents[A] =
  Stream[Parent[A]]

  def loc[A](t: Tree[A], l: TreeForest[A], r: TreeForest[A], p: Parents[A]): TreeLoc[A] =
    new TreeLoc[A] {
      val tree = t
      val lefts = l
      val rights = r
      val parents = p
    }

  def fromForest[A](ts: TreeForest[A]) = ts match {
    case (Stream.cons(t, ts)) => Some(loc(t, Stream.Empty, ts, Stream.Empty))
  }
}
