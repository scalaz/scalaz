package scalaz

import TreeLoc._
import annotation.tailrec

/**
 * A rose-tree zipper. Represents a [[scalaz.Tree]] together with a position in that tree.
 * Provides navigation, persistent update, insertion, and deletes.
 *
 * @param tree The currently selected node.
 * @param lefts The left siblings of the current node.
 * @param rights The right siblings of the current node.
 * @param parents The parent contexts of the current node.
 */
final case class TreeLoc[A](tree: Tree[A], lefts: TreeForest[A],
                            rights: TreeForest[A], parents: Parents[A]) {

  import Tree._

  /** Select the parent of the current node. */
  def parent: Option[TreeLoc[A]] = parents match {
    case (pls, v, prs) #:: ps => Some(loc(Node(v, combChildren(lefts, tree, rights)), pls, prs, ps))
    case Stream.Empty         => None
  }

  /** Select the root node of the tree. */
  @tailrec
  def root: TreeLoc[A] =
    parent match {
      case Some(z) => z.root
      case None    => this
    }

  /** Select the left sibling of the current node. */
  def left: Option[TreeLoc[A]] = lefts match {
    case t #:: ts     => Some(loc(t, ts, tree #:: rights, parents))
    case Stream.Empty => None
  }

  /** Select the right sibling of the current node. */
  def right: Option[TreeLoc[A]] = rights match {
    case t #:: ts     => Some(loc(t, tree #:: lefts, ts, parents))
    case Stream.Empty => None
  }

  /** Select the leftmost child of the current node. */
  def firstChild: Option[TreeLoc[A]] = tree.subForest match {
    case t #:: ts     => Some(loc(t, Stream.Empty, ts, downParents))
    case Stream.Empty => None
  }

  /** Select the rightmost child of the current node. */
  def lastChild: Option[TreeLoc[A]] = tree.subForest.reverse match {
    case t #:: ts     => Some(loc(t, ts, Stream.Empty, downParents))
    case Stream.Empty => None
  }

  /** Select the nth child of the current node. */
  def getChild(n: Int): Option[TreeLoc[A]] =
    for {lr <- splitChildren(Stream.Empty, tree.subForest, n)
         ls = lr._1
    } yield loc(ls.head, ls.tail, lr._2, downParents)

  /** Select the first immediate child of the current node that satisfies the given predicate. */
  def findChild(p: Tree[A] => Boolean): Option[TreeLoc[A]] = {
    @tailrec
    def split(acc: TreeForest[A], xs: TreeForest[A]): Option[(TreeForest[A], Tree[A], TreeForest[A])] =
      (acc, xs) match {
        case (acc, Stream.cons(x, xs)) => if (p(x)) Some((acc, x, xs)) else split(Stream.cons(x, acc), xs)
        case _                         => None
      }
    for (ltr <- split(Stream.Empty, tree.subForest)) yield loc(ltr._2, ltr._1, ltr._3, downParents)
  }

  /** Select the first descendant node of the current node that satisfies the given predicate. */
  def find(p: TreeLoc[A] => Boolean): Option[TreeLoc[A]] =
    Cobind[TreeLoc].cojoin(this).tree.flatten.find(p)

  /** Get the entire tree represented by this zipper. */
  def toTree: Tree[A] = root.tree

  /** Get the entire forest represented by this zipper. */
  def toForest: TreeForest[A] = combChildren(root.lefts, root.tree, root.rights)

  /** True if the current node is the root node. */
  def isRoot: Boolean = parents.isEmpty

  /** True if the current node has no left siblings. */
  def isFirst: Boolean = lefts.isEmpty

  /** True if the current node has no right siblings. */
  def isLast: Boolean = rights.isEmpty

  /** True if the current node has no children. */
  def isLeaf: Boolean = tree.subForest.isEmpty

  /** True if the current node is not the root node. */
  def isChild: Boolean = !isRoot

  /** True if the current node has children. */
  def hasChildren: Boolean = !isLeaf

  /** Replace the current node with the given one. */
  def setTree(t: Tree[A]): TreeLoc[A] = loc(t, lefts, rights, parents)

  /** Modify the current node with the given function. */
  def modifyTree(f: Tree[A] => Tree[A]): TreeLoc[A] = setTree(f(tree))

  /** Modify the label at the current node with the given function. */
  def modifyLabel(f: A => A): TreeLoc[A] = setLabel(f(getLabel))

  /** Get the label of the current node. */
  def getLabel: A = tree.rootLabel

  /** Set the label of the current node. */
  def setLabel(a: A): TreeLoc[A] = modifyTree((t: Tree[A]) => Node(a, t.subForest))

  /** Insert the given node to the left of the current node and give it focus. */
  def insertLeft(t: Tree[A]): TreeLoc[A] = loc(t, lefts, Stream.cons(tree, rights), parents)

  /** Insert the given node to the right of the current node and give it focus. */
  def insertRight(t: Tree[A]): TreeLoc[A] = loc(t, Stream.cons(tree, lefts), rights, parents)

  /** Insert the given node as the first child of the current node and give it focus. */
  def insertDownFirst(t: Tree[A]): TreeLoc[A] = loc(t, Stream.Empty, tree.subForest, downParents)

  /** Insert the given node as the last child of the current node and give it focus. */
  def insertDownLast(t: Tree[A]): TreeLoc[A] = loc(t, tree.subForest.reverse, Stream.Empty, downParents)

  /** Insert the given node as the nth child of the current node and give it focus. */
  def insertDownAt(n: Int, t: Tree[A]): Option[TreeLoc[A]] =
    for (lr <- splitChildren(Stream.Empty, tree.subForest, n)) yield loc(t, lr._1, lr._2, downParents)

  /** Delete the current node and all its children. */
  def delete: Option[TreeLoc[A]] = rights match {
    case Stream.cons(t, ts) => Some(loc(t, lefts, ts, parents))
    case _                  => lefts match {
      case Stream.cons(t, ts) => Some(loc(t, ts, rights, parents))
      case _                  => for (loc1 <- parent) yield loc1.modifyTree((t: Tree[A]) => Node(t.rootLabel, Stream.Empty))
    }
  }

  /**
   * The path from the focus to the root.
   */
  def path: Stream[A] = getLabel #:: parents.map(_._2)

  /** Maps the given function over the elements. */
  def map[B](f: A => B): TreeLoc[B] = {
    val ff = (_: Tree[A]).map(f)
    TreeLoc.loc(tree map f, lefts map ff, rights map ff,
      parents.map {
        case (l, t, r) => (l map ff, f(t), r map ff)
      })
  }

  def cojoin: TreeLoc[TreeLoc[A]] = {

    val lft = (_: TreeLoc[A]).left
    val rgt = (_: TreeLoc[A]).right
    def dwn[A](tz: TreeLoc[A]): (TreeLoc[A], () => Stream[TreeLoc[A]]) = {
      val f = () => std.stream.unfold(tz.firstChild) {
        (o: Option[TreeLoc[A]]) => for (c <- o) yield (c, c.right)
      }
      (tz, f)
    }
    def uf[A](a: TreeLoc[A], f: TreeLoc[A] => Option[TreeLoc[A]]): Stream[Tree[TreeLoc[A]]] = {
      std.stream.unfold(f(a)) {
        (o: Option[TreeLoc[A]]) => for (c <- o) yield (Tree.unfoldTree(c)(dwn[A](_: TreeLoc[A])), f(c))
      }
    }

    val p = std.stream.unfold(parent) {
      (o: Option[TreeLoc[A]]) => for (z <- o) yield ((uf(z, lft), z, uf(z, rgt)), z.parent)
    }
    TreeLoc.loc(Tree.unfoldTree(this)(dwn[A](_: TreeLoc[A])), uf(this, lft), uf(this, rgt), p)
  }

  private def downParents = (lefts, tree.rootLabel, rights) #:: parents

  private def combChildren[A](ls: Stream[A], t: A, rs: Stream[A]) =
    ls.foldLeft(t #:: rs)((a, b) => b #:: a)

  @tailrec
  private def splitChildren[A](acc: Stream[A], xs: Stream[A], n: Int): Option[(Stream[A], Stream[A])] =
    (acc, xs, n) match {
      case (acc, xs, 0)                 => Some((acc, xs))
      case (acc, Stream.cons(x, xs), n) => splitChildren(Stream.cons(x, acc), xs, n - 1)
      case _                            => None
    }
}

sealed abstract class TreeLocInstances {
  implicit val treeLocInstance: Comonad[TreeLoc] with Traverse1[TreeLoc] = new Comonad[TreeLoc] with Traverse1[TreeLoc] {
    import Stream.Empty
    import scalaz.std.stream._

    def copoint[A](p: TreeLoc[A]): A = p.tree.rootLabel

    override def map[A, B](fa: TreeLoc[A])(f: A => B): TreeLoc[B] = fa map f

    def cobind[A, B](fa: TreeLoc[A])(f: TreeLoc[A] => B): TreeLoc[B] = map(cojoin(fa))(f)

    override def cojoin[A](a: TreeLoc[A]): TreeLoc[TreeLoc[A]] = a.cojoin

    override def all[A](fa: TreeLoc[A])(f: A => Boolean) =
      Foldable[Tree].all(fa.tree)(f) && ForestT.all(fa.lefts)(f) && ForestT.all(fa.rights)(f) && ParentsT.all(fa.parents)(f)

    override def any[A](fa: TreeLoc[A])(f: A => Boolean) =
      Foldable[Tree].any(fa.tree)(f) || ForestT.any(fa.lefts)(f) || ForestT.any(fa.rights)(f) || ParentsT.any(fa.parents)(f)

    override def foldMap1[A, B](fa: TreeLoc[A])(f: A => B)(implicit B: Semigroup[B]) =
      foldMapLeft1(fa)(f)((b, a) => B.append(b, f(a)))

    override def foldMap[A, B](fa: TreeLoc[A])(f: A => B)(implicit B: Monoid[B]) =
      B.append(
        B.append(
          Foldable[Tree].foldMap(fa.tree)(f),
          ForestT.foldMap(fa.lefts)(f)
        ),
        B.append(
          ForestT.foldMap(fa.rights)(f),
          ParentsT.foldMap(fa.parents)(f)
        )
      )

    override def foldLeft[A, B](fa: TreeLoc[A], z: B)(f: (B, A) => B) =
      ParentsT.foldLeft(
        fa.parents, ForestT.foldLeft(
          fa.rights, ForestT.foldLeft(
            fa.lefts, Foldable[Tree].foldLeft(fa.tree, z)(f))(f))(f))(f)

    override def foldMapLeft1[A, B](fa: TreeLoc[A])(z: A => B)(f: (B, A) => B) =
      ParentsT.foldLeft(
        fa.parents, ForestT.foldLeft(
          fa.rights, ForestT.foldLeft(
            fa.lefts, Foldable1[Tree].foldMapLeft1(fa.tree)(z)(f))(f))(f))(f)

    override def traverse1Impl[G[_], A, B](fa: TreeLoc[A])(f: A => G[B])(implicit G: Apply[G]) = fa.lefts match {
      case l #:: ls =>
        val lefts1 = ForestT1.traverse1(OneAnd(l, ls))(f)
        fa.rights match {
          case r #:: rs =>
            val rights1 = ForestT1.traverse1(OneAnd(r, rs))(f)
            fa.parents match {
              case p #:: ps =>
                G.apply4(fa.tree.traverse1(f), lefts1, rights1, ParentsT1.traverse1(OneAnd(p, ps))(f))(
                  (tree, lefts, rights, parents) =>
                    TreeLoc(
                      tree = tree,
                      lefts = lefts.head #:: lefts.tail,
                      rights = rights.head #:: rights.tail,
                      parents = parents.head #:: parents.tail
                    )
                )
              case Empty =>
                G.apply3(fa.tree.traverse1(f), lefts1, rights1)(
                  (tree, lefts, rights) =>
                    TreeLoc(
                      tree = tree,
                      lefts = lefts.head #:: lefts.tail,
                      rights = rights.head #:: rights.tail,
                      parents = Empty
                    )
                )
            }
          case Empty =>
            fa.parents match {
              case p #:: ps =>
                G.apply3(fa.tree.traverse1(f), lefts1, ParentsT1.traverse1(OneAnd(p, ps))(f))(
                  (tree, lefts, parents) =>
                    TreeLoc(
                      tree = tree,
                      lefts = lefts.head #:: lefts.tail,
                      rights = Empty,
                      parents = parents.head #:: parents.tail
                    )
                )
              case Empty =>
                G.apply2(fa.tree.traverse1(f), lefts1)(
                  (tree, lefts) =>
                    TreeLoc(
                      tree = tree,
                      lefts = lefts.head #:: lefts.tail,
                      rights = Empty,
                      parents = Empty
                    )
                )
            }
        }
      case Empty =>
        fa.rights match {
          case r #:: rs =>
            val rights1 = ForestT1.traverse1(OneAnd(r, rs))(f)
            fa.parents match {
              case p #:: ps =>
                G.apply3(fa.tree.traverse1(f), rights1, ParentsT1.traverse1(OneAnd(p, ps))(f))(
                  (tree, rights, parents) =>
                    TreeLoc(
                      tree = tree,
                      lefts = Empty,
                      rights = rights.head #:: rights.tail,
                      parents = parents.head #:: parents.tail
                    )
                )
              case Empty =>
                G.apply2(fa.tree.traverse1(f), rights1)(
                  (tree, rights) =>
                    TreeLoc(
                      tree = tree,
                      lefts = Empty,
                      rights = rights.head #:: rights.tail,
                      parents = Empty
                    )
                )
            }
          case Empty =>
            fa.parents match {
              case p #:: ps =>
                G.apply2(fa.tree.traverse1(f), ParentsT1.traverse1(OneAnd(p, ps))(f))(
                  (tree, parents) =>
                    TreeLoc(
                      tree = tree,
                      lefts = Empty,
                      rights = Empty,
                      parents = parents.head #:: parents.tail
                    )
                )
              case Empty =>
                G.map(fa.tree.traverse1(f))(t =>
                  TreeLoc(t, Empty, Empty, Empty)
                )
            }
        }
    }

    override def foldMapRight1[A, B](fa: TreeLoc[A])(z: A => B)(f: (A, => B) => B) =
      ParentsT.foldMapRight1Opt(fa.parents)(z)(f) match {
        case Some(p) =>
          fa.tree.foldRight(
            ForestT.foldRight(fa.lefts, ForestT.foldRight(fa.rights, p)(f))(f)
          )(f)
        case None =>
          ForestT.foldMapRight1Opt(fa.rights)(z)(f) match {
            case Some(r) =>
              fa.tree.foldRight(ForestT.foldRight(fa.lefts, r)(f))(f)
            case None =>
              ForestT.foldMapRight1Opt(fa.lefts)(z)(f) match {
                case Some(l) =>
                  fa.tree.foldRight(l)(f)
                case None =>
                  Foldable1[Tree].foldMapRight1(fa.tree)(z)(f)
              }
          }
      }

    private[this] val ForestT: Traverse[TreeForest] =
      Traverse[Stream].compose[Tree]

    private[this] val ForestT1: Traverse1[λ[a => OneAnd[Stream, Tree[a]]]] =
      Traverse1[OneAnd[Stream, *]].compose[Tree]

    private[this] implicit val ParentT: Traverse1[Parent] = new Traverse1[Parent] {

      override def all[A](fa: Parent[A])(f: A => Boolean) =
        ForestT.all(fa._1)(f) && f(fa._2) && ForestT.all(fa._3)(f)

      override def any[A](fa: Parent[A])(f: A => Boolean) =
        ForestT.any(fa._1)(f) || f(fa._2) || ForestT.any(fa._3)(f)

      override def foldLeft[A, B](fa: Parent[A], z: B)(f: (B, A) => B) =
        ForestT.foldLeft(fa._3, f(ForestT.foldLeft(fa._1, z)(f), fa._2))(f)

      override def foldMap[A, B](fa: Parent[A])(f: A => B)(implicit B: Monoid[B]) =
        B.append(B.append(ForestT.foldMap(fa._1)(f), f(fa._2)), ForestT.foldMap(fa._3)(f))

      override def traverse1Impl[G[_], A, B](fa: Parent[A])(f: A => G[B])(implicit G: Apply[G]): G[Parent[B]] = {
        (fa._1, fa._3) match {
          case (x #:: xs, y #:: ys) =>
            G.apply3(ForestT1.traverse1(OneAnd(x, xs))(f), f(fa._2), ForestT1.traverse1(OneAnd(y, ys))(f)) {
              case (l, c, r) =>
                (l.head #:: l.tail, c, r.head #:: r.tail)
            }
          case (x #:: xs, _) =>
            G.apply2(ForestT1.traverse1(OneAnd(x, xs))(f), f(fa._2)) {
              case (l, c) =>
                (l.head #:: l.tail, c, Empty)
            }
          case (_, x #:: xs) =>
            G.apply2(f(fa._2), ForestT1.traverse1(OneAnd(x, xs))(f)) {
              case (c, r) =>
                (Empty, c, r.head #:: r.tail)
            }
          case (Empty, Empty) =>
            G.map(f(fa._2))(c => (Empty, c, Empty))
        }
      }

      override def foldMapRight1[A, B](fa: Parent[A])(z: A => B)(f: (A, => B) => B): B =
        ForestT.foldMapRight1Opt(fa._3)(z)(f) match {
          case Some(r) =>
            ForestT.foldRight(fa._1, f(fa._2, r))(f)
          case None =>
            ForestT.foldRight(fa._1, z(fa._2))(f)
        }
    }

    private[this] val ParentsT: Traverse[Parents] =
      Traverse[Stream].compose[Parent]

    private[this] val ParentsT1: Traverse1[λ[a => OneAnd[Stream, Parent[a]]]] =
      Traverse1[OneAnd[Stream, *]].compose[Parent]
  }


  implicit def treeLocEqual[A](implicit A: Equal[A]): Equal[TreeLoc[A]] =
    new TreeLocEqual[A] { def E = A }

  implicit def treeLocOrder[A](implicit A: Order[A]): Order[TreeLoc[A]] =
    new Order[TreeLoc[A]] with TreeLocEqual[A] {
      def E = A
      import std.stream._, std.tuple._

      override def order(a: TreeLoc[A], b: TreeLoc[A]) =
        Divide[Order].deriving4(
          (x: TreeLoc[A]) => (x.tree, x.lefts, x.rights, x.parents)
        ).order(a, b)
    }
}

object TreeLoc extends TreeLocInstances {
  type TreeForest[A] =
  Stream[Tree[A]]

  type Parent[A] =
  (TreeForest[A], A, TreeForest[A])

  type Parents[A] =
  Stream[Parent[A]]

  def loc[A](t: Tree[A], l: TreeForest[A], r: TreeForest[A], p: Parents[A]): TreeLoc[A] =
    TreeLoc(t, l, r, p)

  def fromForest[A](ts: TreeForest[A]) = ts match {
    case (Stream.cons(t, ts)) => Some(loc(t, Stream.Empty, ts, Stream.Empty))
  }
}

private trait TreeLocEqual[A] extends Equal[TreeLoc[A]] {
  implicit def E: Equal[A]
  import std.stream._, std.tuple._

  override final def equal(a: TreeLoc[A], b: TreeLoc[A]) = {
    Equal[Tree[A]].equal(a.tree, b.tree) &&
    Equal[TreeForest[A]].equal(a.lefts, b.lefts) &&
    Equal[TreeForest[A]].equal(a.rights, b.rights) &&
    Equal[Parents[A]].equal(a.parents, b.parents)
  }
}
