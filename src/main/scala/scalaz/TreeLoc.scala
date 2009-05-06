package scalaz

import S._
import OptionW._
import MA._

sealed trait TreeLoc[+A] {
  val tree: Tree[A]
  val lefts: Stream[Tree[A]]
  val rights: Stream[Tree[A]]
  val parents: Stream[(Stream[Tree[A]], A, Stream[Tree[A]])]

  import Tree._
  import TreeLoc._
  def parent = parents match {
    case Stream.cons((pls, v, prs), ps) => Some(loc(node(v, combChildren(lefts, tree, rights)), pls, prs, ps))
    case Stream.empty => None
  }

  def root: TreeLoc[A] = parent.some(_.root).none(this)

  def left = lefts match {
    case Stream.cons(t, ts) => Some(loc(t, ts, tree lazy_:: rights, parents))
    case Stream.empty => None
  }

  def right = rights match {
    case Stream.cons(t, ts) => Some(loc(t, tree lazy_:: lefts, ts, parents))
    case Stream.empty => None
  }

  def firstChild = tree.subForest match {
    case Stream.cons(t, ts) => Some(loc(t, Stream.empty, ts, downParents))
    case Stream.empty => None
  }

  def lastChild = tree.subForest.reverse match {
    case Stream.cons(t, ts) => Some(loc(t, ts, Stream.empty, downParents))
    case Stream.empty => None
  }

  def getChild(n: Int) =
    for (val lr <- splitChildren(Stream.empty, tree.subForest, n)) {
      val ls = lr._1
      loc(ls.head, ls.tail, lr._2, downParents)
    }

  def findChild(p: Tree[A] => Boolean) = {
    def split(acc: Stream[Tree[A]], xs: Stream[Tree[A]]): Option[(Stream[Tree[A]], Tree[A], Stream[Tree[A]])] =
      (acc, xs) match {
        case (acc, Stream.cons(x, xs)) => if (p(x)) Some((acc, x, xs)) else split(Stream.cons(x, acc), xs)
        case _ => None
      }
    for (val ltr <- split(Stream.empty, tree.subForest)) yield loc(ltr._2, ltr._1, ltr._3, downParents)
  }

  def toTree = root.tree

  def toForest = combChildren(root.lefts, root.tree, root.rights)

  def isRoot = parents.isEmpty

  def isFirst = lefts.isEmpty

  def isLast = rights.isEmpty

  def isLeaf = tree.subForest.isEmpty

  def isChild = !isRoot

  def hasChildren = !isLeaf

  def setTree[AA >: A](t: Tree[AA]) = loc(t, lefts, rights, parents)

  def modifyTree[AA >: A](f: Tree[AA] => Tree[AA]) = setTree(f(tree))

  def modifyLabel[AA >: A](f: AA => AA) = setLabel(f(getLabel))

  def getLabel = tree.rootLabel

  def setLabel[AA >: A](a: AA) = modifyTree((t: Tree[AA]) => node(a, t.subForest))

  def insertLeft[AA >: A](t: Tree[AA]) = loc(t, lefts, Stream.cons(tree, rights), parents)

  def insertRight[AA >: A](t: Tree[AA]) = loc(t, Stream.cons(tree, lefts), rights, parents)

  def insertDownFirst[AA >: A](t: Tree[AA]) = loc(t, Stream.empty, tree.subForest, downParents)

  def insertDownLast[AA >: A](t: Tree[AA]) = loc(t, tree.subForest.reverse, Stream.empty, downParents)

  def insertDownAt[AA >: A](n: Int, t: Tree[AA]) =
    for (lr <- splitChildren(Stream.empty, tree.subForest, n)) yield loc(t, lr._1, lr._2, downParents)

  def delete = rights match {
    case Stream.cons(t, ts) => Some(loc(t, lefts, ts, parents))
    case _ => lefts match {
      case Stream.cons(t, ts) => Some(loc(t, ts, rights, parents))
      case _ => for (val loc1 <- parent) yield loc1.modifyTree((t: Tree[A]) => node(t.rootLabel, Stream.empty))
    }
  }

  private def downParents = (lefts, tree.rootLabel, rights) lazy_:: parents
}

object TreeLoc {
  def loc[A](t: Tree[A], l: Stream[Tree[A]], r: Stream[Tree[A]], p: Stream[(Stream[Tree[A]], A, Stream[Tree[A]])]) =
    new TreeLoc[A] {
      val tree = t
      val lefts = l
      val rights = r
      val parents = p
    }

  def fromForest[A](ts: Stream[Tree[A]]) = ts match {
    case (Stream.cons(t, ts)) => Some(loc(t, Stream.empty, ts, Stream.empty))
  }

  private def combChildren[A](ls: Stream[A], t: A, rs: Stream[A]) =
    ls.foldl(t lazy_:: rs, ((_: A) lazy_:: (_: Stream[A])).flip)

  private def splitChildren[A](acc: Stream[A], xs: Stream[A], n: Int): Option[(Stream[A], Stream[A])] =
    (acc, xs, n) match {
      case (acc, xs, 0) => Some((acc, xs))
      case (acc, Stream.cons(x, xs), n) => splitChildren(Stream.cons(x, acc), xs, n - 1)
      case _ => None
    }
}