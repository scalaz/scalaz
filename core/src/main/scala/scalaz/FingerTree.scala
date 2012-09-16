package scalaz

import collection.Iterator
import syntax.semigroup._
import syntax.reducer._
import std.option.optionSyntax._
import syntax.Ops


/**View of the left end of a sequence.*/
sealed abstract class ViewL[S[_], A] {
  def fold[B](b: => B, f: (=> A, => S[A]) => B): B
  def headOption: Option[A] = fold(None, (a, sa) => Some(a))
  def tailOption: Option[S[A]] = fold(None, (a, sa) => Some(sa))
  def head: A = headOption.getOrElse(sys.error("Head on empty view"))
  def tail: S[A] = tailOption.getOrElse(sys.error("Tail on empty view"))
}

/**View of the right end of a sequence.*/
sealed abstract class ViewR[S[_], A] {
  def fold[B](b: => B, f: (=> S[A], => A) => B): B
  def lastOption: Option[A] = fold(None, (sa, a) => Some(a))
  def initOption: Option[S[A]] = fold(None, (sa, a) => Some(sa))
  def last: A = lastOption.getOrElse(sys.error("Last on empty view"))
  def init: S[A] = initOption.getOrElse(sys.error("Init on empty view"))
}

import FingerTree._
import std.option._

sealed abstract class Finger[V, A] {
  def foldMap[B](f: A => B)(implicit m: Semigroup[B]): B

  /**
   * Append the given element to the right
   *
   * @throws if the finger is `Four`.
   */
  def +:(a: => A): Finger[V, A]

  /**
   * Prepends the given element to the left
   *
   * @throws if the finger is `Four`.
   */
  def :+(a: => A): Finger[V, A]

  /** Replaces the first element of this finger with `a` */
  def |-:(a: => A): Finger[V, A]

  /** Replaces the last element of this finger with `a` */
  def :-|(a: => A): Finger[V, A]

  def lhead: A

  def ltail: Finger[V, A]

  def rhead: A

  def rtail: Finger[V, A]

  def toTree: FingerTree[V, A]

  def map[B, V2](f: A => B)(implicit m: Reducer[B, V2]): Finger[V2, B]

  /** Apply the given side effect to each element. */
  def foreach(f: A => Unit): Unit

  /** An iterator that visits each element. */
  def iterator: Iterator[A]

  /** An iterator that visits each element in reverse order. */
  def reverseIterator: Iterator[A]

  def measure: V

  def toList: List[A] = map(x => x)(Reducer.ListReducer[A]).measure

  private[scalaz] def split1(pred: V => Boolean, accV: V): (Option[Finger[V, A]], A, Option[Finger[V, A]])
}
case class One[V, A](v: V, a1: A)(implicit r: Reducer[A, V]) extends Finger[V, A] {

  def foldMap[B](f: A => B)(implicit m: Semigroup[B]) = f(a1)

  def +:(a: => A) = Two(a cons v, a, a1)

  def :+(a: => A) = Two(v snoc a, a1, a)

  def |-:(a: => A) = one(a)

  def :-|(a: => A) = one(a)

  def lhead = a1

  def ltail = sys.error("Tail on the digit One")

  def rhead = a1

  def rtail = sys.error("Tail on the digit One")

  def toTree = single(a1)

  def map[B, V2](f: A => B)(implicit r: Reducer[B, V2]) = one(f(a1))

  def foreach(f: A => Unit) {
    f(a1)
  }

  def iterator = Iterator.single(a1)

  def reverseIterator = Iterator.single(a1)

  val measure = v

  private[scalaz] def split1(pred: V => Boolean, accV: V) = (None, a1, None)
}


case class Two[V, A](v: V, a1: A, a2: A)(implicit r: Reducer[A, V]) extends Finger[V, A] {
  def foldMap[B](f: A => B)(implicit m: Semigroup[B]) = f(a1) |+| f(a2)

  def +:(a: => A) = Three(a cons v, a, a1, a2)

  def :+(a: => A) = Three(v snoc a, a1, a2, a)

  def |-:(a: => A) = two(a, a2)

  def :-|(a: => A) = two(a1, a)

  def lhead = a1

  def ltail = one(a2)

  def rhead = a2

  def rtail = one(a1)

  def toTree = {
    deep(v, one(a1), empty[V, Node[V, A]], one(a2))
  }

  def map[B, V2](f: A => B)(implicit r: Reducer[B, V2]) = two(f(a1), f(a2))

  def foreach(f: A => Unit) {
    f(a1)
    f(a2)
  }

  def iterator = Iterator(a1, a2)

  def reverseIterator = Iterator(a2, a1)

  val measure = v

  private implicit def sg: Semigroup[V] = r.monoid

  private[scalaz] def split1(pred: V => Boolean, accV: V) = {
    val va1 = r.unit(a1)
    val accVa1 = accV |+| va1
    if (pred(accVa1))
      (None, a1, Some(one(a2)))
    else
      (Some(One(va1, a1)), a2, None)
  }
}
case class Three[V, A](v: V, a1: A, a2: A, a3: A)(implicit r: Reducer[A, V]) extends Finger[V, A] {
  def foldMap[B](f: A => B)(implicit m: Semigroup[B]) = f(a1) |+| f(a2) |+| f(a3)

  def +:(a: => A) = Four(a cons v, a, a1, a2, a3)

  def :+(a: => A) = Four(v snoc a, a1, a2, a3, a)

  def |-:(a: => A) = three(a, a2, a3)

  def :-|(a: => A) = three(a1, a2, a)

  def lhead = a1

  def ltail = two(a2, a3)

  def rhead = a3

  def rtail = two(a1, a2)

  def toTree = {
    deep(v, two(a1, a2), empty[V, Node[V, A]], one(a3))
  }

  def map[B, V2](f: A => B)(implicit r: Reducer[B, V2]) = three(f(a1), f(a2), f(a3))

  def foreach(f: A => Unit) {
    f(a1)
    f(a2)
    f(a3)
  }

  def iterator = Iterator(a1, a2, a3)

  def reverseIterator = Iterator(a3, a2, a1)

  val measure = v

  private implicit def sg: Semigroup[V] = r.monoid

  private[scalaz] def split1(pred: V => Boolean, accV: V) = {
    val va1 = r.unit(a1)
    val accVa1 = accV |+| va1
    if (pred(accVa1))
      (None, a1, Some(two(a2, a3)))
    else {
      val accVa2 = accVa1 snoc a2
      if (pred(accVa2))
        (Some(One(va1, a1)), a2, Some(one(a3)))
      else
        (Some(two(a1, a2)), a3, None)
    }
  }
}
case class Four[V, A](v: V, a1: A, a2: A, a3: A, a4: A)(implicit r: Reducer[A, V]) extends Finger[V, A] {
  def foldMap[B](f: A => B)(implicit m: Semigroup[B]) = f(a1) |+| f(a2) |+| f(a3) |+| f(a4)

  def +:(a: => A) = sys.error("Digit overflow")

  def :+(a: => A) = sys.error("Digit overflow")

  def |-:(a: => A) = four(a, a2, a3, a4)

  def :-|(a: => A) = four(a1, a2, a3, a)

  def lhead = a1

  def ltail = three(a2, a3, a4)

  def rhead = a4

  def rtail = three(a1, a2, a3)

  def toTree = {
    deep(v, two(a1, a2), empty[V, Node[V, A]], two(a3, a4))
  }

  def map[B, V2](f: A => B)(implicit r: Reducer[B, V2]) = four(f(a1), f(a2), f(a3), f(a4))

  def foreach(f: A => Unit) {
    f(a1)
    f(a2)
    f(a3)
    f(a4)
  }

  def iterator = Iterator(a1, a2, a3, a4)

  def reverseIterator = Iterator(a4, a3, a2, a1)

  val measure = v

  private implicit def sg: Semigroup[V] = r.monoid

  private[scalaz] def split1(pred: V => Boolean, accV: V) = {
    val va1 = r.unit(a1)
    val accVa1 = accV |+| va1
    if (pred(accVa1))
      (None, a1, Some(three(a2, a3, a4)))
    else {
      val accVa2 = accVa1 snoc a2
      if (pred(accVa2))
        (Some(One(va1, a1)), a2, Some(two(a3, a4)))
      else {
        val accVa3 = accVa2 snoc a3
        if (pred(accVa3))
          (Some(two(a1, a2)), a3, Some(one(a4)))
        else
          (Some(three(a1, a2, a3)), a4, None)
      }
    }
  }
}

sealed abstract class Node[V, A](implicit r: Reducer[A, V]) {
  def fold[B](two: (V, => A, => A) => B, three: (V, => A, => A, => A) => B): B

  def foldMap[B](f: A => B)(implicit m: Semigroup[B]): B = fold(
    (v, a1, a2) => f(a1) |+| f(a2),
    (v, a1, a2, a3) => f(a1) |+| f(a2) |+| f(a3))

  def toDigit = fold(
    (v, a1, a2) => Two(v, a1, a2),
    (v, a1, a2, a3) => Three(v, a1, a2, a3))

  val measure: V

  def map[B, V2](f: A => B)(implicit m: Reducer[B, V2]) = fold(
    (v, a1, a2) => node2(f(a1), f(a2)),
    (v, a1, a2, a3) => node3(f(a1), f(a2), f(a3)))

  def foreach(f: A => Unit) {
    fold(
      (_, a1, a2) => { f(a1); f(a2) },
      (_, a1, a2, a3) => { f(a1); f(a2); f(a3) }
    )}

  def iterator = fold(
    (_, a1, a2) => Iterator(a1, a2),
    (_, a1, a2, a3) => Iterator(a1, a2, a3))

  def reverseIterator = fold(
    (_, a1, a2) => Iterator(a2, a1),
    (_, a1, a2, a3) => Iterator(a3, a2, a1))

  private implicit def sg: Semigroup[V] = r.monoid

  private[scalaz] def split1(pred: V => Boolean, accV: V): (Option[Finger[V, A]], A, Option[Finger[V, A]]) = fold(
    (v, a1, a2) => {
      val va1 = r.unit(a1)
      val accVa1 = accV |+| va1
      if (pred(accVa1))
        (None, a1, Some(one(a2)))
      else
        (Some(One(va1, a1)), a2, None)
    },
    (v, a1, a2, a3) => {
      val va1 = r.unit(a1)
      val accVa1 = accV |+| va1
      if (pred(accVa1))
        (None, a1, Some(two(a2, a3)))
      else {
        val accVa2 = accVa1 snoc a2
        if (pred(accVa2))
          (Some(One(va1, a1)), a2, Some(one(a3)))
        else
          (Some(two(a1, a2)), a3, None)
      }
    })
}

/**
 * Finger trees with leaves of type A and Nodes that are annotated with type V.
 *
 * Finger Trees provide a base for implementations of various collection types,
 * as described in "Finger trees: a simple general-purpose data structure", by
 * Ralf Hinze and Ross Paterson.
 * A gentle introduction is presented in the blog post "Monoids and Finger Trees" by Heinrich Apfelmus.
 *
 * This is done by choosing a a suitable type to annotate the nodes. For example,
 * a binary tree can be implemented by annotating each node with the size of its subtree,
 * while a priority queue can be implemented by labelling the nodes by the minimum priority of its children.
 *
 * The operations on FingerTree enforce the constraint measured (in the form of a Reducer instance).
 *
 * Finger Trees have excellent (amortized) asymptotic performance:
 *
 *  - Access to the first and last elements is `O(1)`
 *  - Appending/prepending a single value is `O(1)`
 *  - Concatenating two trees is `(O lg min(l1, l2))` where `l1` and `l2` are their sizes
 *  - Random access to an element at `n` is `O(lg min(n, l - n))`, where `l` is the size of the tree.
 *  - Constructing a tree with n copies of a value is O(lg n).
 *
 * @tparam V The type of the annotations of the nodes (the '''measure''')
 * @tparam A The type of the elements stored at the leaves
 *
 * @see [[http://www.soi.city.ac.uk/~ross/papers/FingerTree.pdf Finger trees: a simple general-purpose data structure]]
 * @see [[http://apfelmus.nfshost.com/articles/monoid-fingertree.html]]
 */
sealed abstract class FingerTree[V, A](implicit measurer: Reducer[A, V]) {
  def measure: V = this.unit[V]

  def foldMap[B](f: A => B)(implicit s: Monoid[B]): B =
    fold(v => s.zero, (v, x) => f(x), (v, pr, m, sf) => pr.foldMap(f) |+| m.foldMap(x => x.foldMap(f)) |+| sf.foldMap(f))

  def foldRight[B](z: => B)(f: (A, => B) => B): B = {
    foldMap((a: A) => (Endo.endo(f(a, _: B)))) apply z
  }

  def foldLeft[B](b: B)(f: (B, A) => B): B = {
    fold(v => b,
          (v, a) => f(b, a),
          (v, pr, m, sf) =>
              fingerFoldable[V].foldLeft(sf, m.foldLeft[B](fingerFoldable[V].foldLeft(pr, b)(f))((x, y) => nodeFoldable[V].foldLeft(y, x)(f)))(f))
  }

  /**
   * Fold over the structure of the tree. The given functions correspond to the three possible variations of the finger tree.
   *
   * @param empty if the tree is empty, convert the measure to a `B`
   * @param single if the tree contains a single element, convert the measure and this element to a `B`
   * @param deep otherwise, convert the measure, the two fingers, and the sub tree to a `B`.
   */
  def fold[B](empty: V => B, single: (V, A) => B, deep: (V, Finger[V, A], => FingerTree[V, Node[V, A]], Finger[V, A]) => B): B

  /** Prepends an element to the left of the tree. O(1). */
  def +:(a: => A): FingerTree[V, A] = {
    implicit val nm = nodeMeasure[A, V]
    fold(v => single(a cons v, a), (v, b) => deep(a cons v, one(a), empty[V, Node[V, A]], one(b)), (v, pr, m, sf) => {
      val mz = m
      pr match {
        case Four(vf, b, c, d, e) => deep(a cons v, two(a, b), node3(c, d, e) +: mz, sf)
        case _ => deep(a cons v, a +: pr, mz, sf)
      }})
  }

  /** Appends an element to the right of the tree. O(1). */
  def :+(a: => A): FingerTree[V, A] = {
    implicit val nm = nodeMeasure[A, V]
    fold(v => single(v snoc a, a), (v, b) => deep(v snoc a, one(b), empty[V, Node[V, A]], one(a)), (v, pr, m, sf) => {
      val mz = m
      sf match {
        case Four(vf, b, c, d, e) => deep(v snoc a, pr, (mz :+ node3(b, c, d)), two(e, a))
        case _ => deep(v snoc a, pr, mz, sf :+ a)
      }})
  }

  /** Replace the first element of the tree with the given value. O(1) */
  def |-:(a: => A): FingerTree[V, A] = {
    fold(
      v => sys.error("Replacing first element of an empty FingerTree"),
      (v, b) => single(a),
      (v, pr, m, sf) => deep(a |-: pr, m, sf))
  }

  /** Replace the last element of the tree with the given value. O(1) */
  def :-|(a: => A): FingerTree[V, A] = {
    fold(
      v => sys.error("Replacing last element of an empty FingerTree"),
      (v, b) => single(a),
      (v, pr, m, sf) => deep(pr, m, sf :-| a))
  }

  /** Appends the given finger tree to the right of this tree. */
  def <++>(right: FingerTree[V, A]): FingerTree[V, A] = fold(
      v => right,
      (v, x) => x +: right,
      (v1, pr1, m1, sf1) =>
        right.fold(
          v => this,
          (v, x) => this :+ x,
          (v2, pr2, m2, sf2) => deep(v1 |+| v2, pr1, addDigits0(m1, sf1, pr2, m2), sf2)
        )
    )

  private type ATree = FingerTree[V, A]
  private type AFinger = Finger[V, A]
  private type NodeTree = FingerTree[V, Node[V, A]]

  private implicit def sg: Monoid[V] = measurer.monoid

  def add1(n: A, right: => ATree): ATree = fold(
    v => n +: right,
    (v, x) => x +: n +: right,
    (v1, pr1, m1, sf1) =>
      right.fold(
        v => this :+ n,
        (v, x) => this :+ n :+ x,
        (v2, pr2, m2, sf2) =>
          deep((v1 snoc n) |+| v2, pr1, addDigits1(m1, sf1, n, pr2, m2), sf2)
      )
  )

  def add2(n1: => A, n2: => A, right: => ATree): ATree = fold(
    v => n1 +: n2 +: right,
    (v, x) => x +: n1 +: n2 +: right,
    (v1, pr1, m1, sf1) =>
      right.fold(
        v => this :+ n1 :+ n2,
        (v, x) => this :+ n1 :+ n2 :+ x,
        (v2, pr2, m2, sf2) =>
          deep((v1 snoc n1 snoc n2) |+| v2, pr1, addDigits2(m1, sf1, n1, n2, pr2, m2), sf2)
      )
  )

  def add3(n1: => A, n2: => A, n3: => A, right: => ATree): ATree = fold(
    v => n1 +: n2 +: n3 +: right,
    (v, x) => x +: n1 +: n2 +: n3 +: right,
    (v1, pr1, m1, sf1) =>
      right.fold(
         v => this :+ n1 :+ n2 :+ n3,
         (v, x) => this :+ n1 :+ n2 :+ n3 :+ x,
         (v2, pr2, m2, sf2) =>
           deep((v1 snoc n1 snoc n2 snoc n3) |+| v2,
             pr1, addDigits3(m1, sf1, n1, n2, n3, pr2, m2), sf2)
      )
  )

  def add4(n1: => A, n2: => A, n3: => A, n4: => A, right: => ATree): ATree = fold(
    v => n1 +: n2 +: n3 +: n4 +: right,
    (v, x) => x +: n1 +: n2 +: n3 +: n4 +: right,
    (v1, pr1, m1, sf1) =>
      right.fold(
        v => this :+ n1 :+ n2 :+ n3 :+ n4,
        (v, x) => this :+ n1 :+ n2 :+ n3 :+ n4 :+ x,
        (v2, pr2, m2, sf2) =>
          deep((v1 snoc n1 snoc n2 snoc n3 snoc n4) |+| v2,
            pr1, addDigits4(m1, sf1, n1, n2, n3, n4, pr2, m2), sf2)
      )
  )

  def addDigits0(m1: => NodeTree, dig1: => AFinger, dig2: => AFinger, m2: => NodeTree): NodeTree = dig1 match {
    case One(_, a) => dig2 match {
      case One(_, b) => m1.add1(node2(a, b), m2)
      case Two(_, b,c) => m1.add1(node3(a,b,c), m2)
      case Three(_, b,c,d) => m1.add2(node2(a,b), node2(c,d),m2)
      case Four(_, b,c,d,e) => m1.add2(node3(a,b,c), node2(d,e), m2)
    }
    case Two(_, a,b) => dig2 match {
      case One(_, c) => m1.add1(node3(a,b,c), m2)
      case Two(_, c,d) => m1.add2(node2(a,b), node2(c,d), m2)
      case Three(_, c,d,e) => m1.add2(node3(a,b,c), node2(d,e), m2)
      case Four(_, c,d,e,f) => m1.add2(node3(a,b,c), node3(d,e,f), m2)
    }
    case Three(_, a,b,c) => dig2 match {
      case One(_, d) => m1.add2(node2(a,b), node2(c,d), m2)
      case Two(_, d,e) => m1.add2(node3(a,b,c), node2(d,e), m2)
      case Three(_, d,e,f) => m1.add2(node3(a,b,c), node3(d,e,f), m2)
      case Four(_, d,e,f,g) => m1.add3(node3(a,b,c), node2(d,e), node2(f,g), m2)
    }
    case Four(_, a,b,c,d) => dig2 match {
      case One(_, e) => m1.add2(node3(a,b,c), node2(d,e), m2)
      case Two(_, e,f) => m1.add2(node3(a,b,c), node3(d,e,f), m2)
      case Three(_, e,f,g) => m1.add3(node3(a,b,c), node2(d,e), node2(f,g), m2)
      case Four(_, e,f,g,h) => m1.add3(node3(a,b,c), node3(d,e,f), node2(g,h), m2)
    }
  }

  def addDigits1(m1: => NodeTree, d1: => AFinger, x: => A, d2: => AFinger, m2: => NodeTree): NodeTree = d1 match {
    case One(_, a) => d2 match {
      case One(_, b) => m1.add1(node3(a,x,b), m2)
      case Two(_, b,c) => m1.add2(node2(a,x), node2(b,c), m2)
      case Three(_, b,c,d) => m1.add2(node3(a,x,b), node2(c,d), m2)
      case Four(_, b,c,d,e) => m1.add2(node3(a,x,b), node3(c,d,e), m2)
    }
    case Two(_, a,b) => d2 match {
      case One(_, c) => m1.add2(node2(a,b), node2(x,c), m2)
      case Two(_, c,d) => m1.add2(node3(a,b,x), node2(c,d), m2)
      case Three(_, c,d,e) => m1.add2(node3(a,b,x), node3(c,d,e), m2)
      case Four(_, c,d,e,f) => m1.add3(node3(a,b,x), node2(c,d), node2(e,f), m2)
    }
    case Three(_, a,b,c) => d2 match {
      case One(_, d) => m1.add2(node3(a,b,c), node2(x,d), m2)
      case Two(_, d,e) => m1.add2(node3(a,b,c), node3(x,d,e), m2)
      case Three(_, d,e,f) => m1.add3(node3(a,b,c), node2(x,d), node2(e,f), m2)
      case Four(_, d,e,f,g) => m1.add3(node3(a,b,c), node3(x,d,e), node2(f,g), m2)
    }
    case Four(_, a,b,c,d) => d2 match {
      case One(_, e) => m1.add2(node3(a,b,c), node3(d,x,e), m2)
      case Two(_, e,f) => m1.add3(node3(a,b,c), node2(d,x), node2(e,f), m2)
      case Three(_, e,f,g) => m1.add3(node3(a,b,c), node3(d,x,e), node2(f,g), m2)
      case Four(_, e,f,g,h) => m1.add3(node3(a,b,c), node3(d,x,e), node3(f,g,h), m2)
    }
  }

  def addDigits2(m1: => NodeTree, d1: => AFinger, x: => A, y: => A, d2: => AFinger, m2: => NodeTree): NodeTree = d1 match {
    case One(_, a) => d2 match {
      case One(_, b) => m1.add2(node2(a,x), node2(y,b), m2)
      case Two(_, b,c) => m1.add2(node3(a,x,y), node2(b,c), m2)
      case Three(_, b,c,d) => m1.add2(node3(a,x,y), node3(b,c,d), m2)
      case Four(_, b,c,d,e) => m1.add3(node3(a,x,y), node2(b,c), node2(d,e), m2)
    }
    case Two(_, a,b) => d2 match {
      case One(_, c) => m1.add2(node3(a,b,x), node2(y,c), m2)
      case Two(_, c,d) => m1.add2(node3(a,b,x), node3(y,c,d), m2)
      case Three(_, c,d,e) => m1.add3(node3(a,b,x), node2(y,c), node2(d,e), m2)
      case Four(_, c,d,e,f) => m1.add3(node3(a,b,x), node3(y,c,d), node2(e,f), m2)
    }
    case Three(_, a,b,c) => d2 match {
      case One(_, d) => m1.add2(node3(a,b,c), node3(x,y,d), m2)
      case Two(_, d,e) => m1.add3(node3(a,b,c), node2(x,y), node2(d,e), m2)
      case Three(_, d,e,f) => m1.add3(node3(a,b,c), node3(x,y,d), node2(e,f), m2)
      case Four(_, d,e,f,g) => m1.add3(node3(a,b,c), node3(x,y,d), node3(e,f,g), m2)
    }
    case Four(_, a,b,c,d) => d2 match {
      case One(_, e) => m1.add3(node3(a,b,c), node2(d,x), node2(y,e), m2)
      case Two(_, e,f) => m1.add3(node3(a,b,c), node3(d,x,y), node2(e,f), m2)
      case Three(_, e,f,g) => m1.add3(node3(a,b,c), node3(d,x,y), node3(e,f,g), m2)
      case Four(_, e,f,g,h) => m1.add4(node3(a,b,c), node3(d,x,y), node2(e,f), node2(g,h), m2)
    }
  }

  def addDigits3(m1: => NodeTree, d1: => AFinger, x: => A, y: => A, z: => A, d2: => AFinger, m2: => NodeTree): NodeTree = d1 match {
    case One(_, a) => d2 match {
      case One(_, b) => m1.add2(node3(a,x,y), node2(z,b), m2)
      case Two(_, b,c) => m1.add2(node3(a,x,y), node3(z,b,c), m2)
      case Three(_, b,c,d) => m1.add3(node3(a,x,y), node2(z,b), node2(c,d), m2)
      case Four(_, b,c,d,e) => m1.add3(node3(a,x,y), node3(z,b,c), node2(d,e), m2)
    }
    case Two(_, a,b) => d2 match {
      case One(_, c) => m1.add2(node3(a,b,x), node3(y,z,c), m2)
      case Two(_, c,d) => m1.add3(node3(a,b,x), node2(y,z), node2(c,d), m2)
      case Three(_, c,d,e) => m1.add3(node3(a,b,x), node3(y,z,c), node2(d,e), m2)
      case Four(_, c,d,e,f) => m1.add3(node3(a,b,x), node3(y,z,c), node3(d,e,f),m2)
    }
    case Three(_, a,b,c) => d2 match {
      case One(_, d) => m1.add3(node3(a,b,c), node2(x,y), node2(z,d), m2)
      case Two(_, d,e) => m1.add3(node3(a,b,c), node3(x,y,z), node2(d,e), m2)
      case Three(_, d,e,f) => m1.add3(node3(a,b,c), node3(x,y,z), node3(d,e,f), m2)
      case Four(_, d,e,f,g) => m1.add4(node3(a,b,c), node3(x,y,z), node2(d,e), node2(f,g), m2)
    }
    case Four(_, a,b,c,d) => d2 match {
      case One(_, e) => m1.add3(node3(a,b,c), node3(d,x,y), node2(z,e), m2)
      case Two(_, e,f) => m1.add3(node3(a,b,c), node3(d,x,y), node3(z,e,f), m2)
      case Three(_, e,f,g) => m1.add4(node3(a,b,c), node3(d,x,y), node2(z,e),node2(f,g), m2)
      case Four(_, e,f,g,h) => m1.add4(node3(a,b,c), node3(d,x,y), node3(z,e,f), node2(g,h), m2)
    }
  }

  def addDigits4(m1: => NodeTree, d1: => AFinger, x: => A, y: => A, z: => A, w: => A, d2: => AFinger, m2: => NodeTree): NodeTree = d1 match {
    case One(_, a) => d2 match {
      case One(_, b) => m1.add2(node3(a,x,y), node3(z,w,b), m2)
      case Two(_, b,c) => m1.add3(node3(a,x,y), node2(z,w), node2(b,c), m2)
      case Three(_, b,c,d) => m1.add3(node3(a,x,y), node3(z,w,b), node2(c,d), m2)
      case Four(_, b,c,d,e) => m1.add3(node3(a,x,y), node3(z,w,b), node3(c,d,e), m2)
    }
    case Two(_, a,b) => d2 match {
      case One(_, c) => m1.add3(node3(a,b,x), node2(y,z), node2(w,c), m2)
      case Two(_, c,d) => m1.add3(node3(a,b,x), node3(y,z,w), node2(c,d), m2)
      case Three(_, c,d,e) => m1.add3(node3(a,b,x), node3(y,z,w), node3(c,d,e), m2)
      case Four(_, c,d,e,f) => m1.add4(node3(a,b,x), node3(y,z,w), node2(c,d), node2(e,f),m2)
    }
    case Three(_, a,b,c) => d2 match {
      case One(_, d) => m1.add3(node3(a,b,c), node3(x,y,z), node2(w,d), m2)
      case Two(_, d,e) => m1.add3(node3(a,b,c), node3(x,y,z), node3(w,d,e), m2)
      case Three(_, d,e,f) => m1.add4(node3(a,b,c), node3(x,y,z), node2(w,d),node2(e,f), m2)
      case Four(_, d,e,f,g) => m1.add4(node3(a,b,c), node3(x,y,z), node3(w,d,e), node2(f,g), m2)
    }
    case Four(_, a,b,c,d) => d2 match {
      case One(_, e) => m1.add3(node3(a,b,c), node3(d,x,y), node3(z,w,e), m2)
      case Two(_, e,f) => m1.add4(node3(a,b,c), node3(d,x,y), node2(z,w), node2(e,f), m2)
      case Three(_, e,f,g) => m1.add4(node3(a,b,c), node3(d,x,y), node3(z,w,e),node2(f,g), m2)
      case Four(_, e,f,g,h) => m1.add4(node3(a,b,c), node3(d,x,y), node3(z,w,e), node3(f,g,h), m2)
    }
  }

  /**
   * Splits this tree into a pair of subtrees at the point where the given predicate, based on the measure,
   * changes from `true` to `false`. O(log(min(i,n-i)))
   *
   * @return `(as, bs)` `as`: the subtree containing elements before the point where `pred` first holds
   *                    `fs` the subtree containing element at and after the point where `pred` first holds. Empty if `pred` never holds.
   */
  def split(pred: V => Boolean): (FingerTree[V, A], FingerTree[V, A]) =
    if (!isEmpty && pred(measure)) {
      val (l, x, r) = split1(pred)
      (l, x +: r)
    }
    else
      (this, empty)

  /**
   * Like `split`, but returns the element where `pred` first holds separately
   *
   * @throws if the tree is empty.
   */
  def split1(pred: V => Boolean): (FingerTree[V, A], A, FingerTree[V, A]) = split1(pred, measurer.monoid.zero)

  private def split1(pred: V => Boolean, accV: V): (FingerTree[V, A], A, FingerTree[V, A]) = fold(
    v => sys.error("Splitting an empty FingerTree"), // we can never get here
    (v, x) => (empty, x, empty),
    (v, pr, m, sf) => {
      val accVpr = accV snoc pr
      if (pred(accVpr)) {
        val (l, x, r) = pr.split1(pred, accV)
        (l.cata(_.toTree, empty), x, deepL(r, m, sf))
      } else {
        val accVm = mappendVal(accVpr, m)
        if (pred(accVm)) {
          val (ml, xs, mr) = m.split1(pred, accVpr)
          val (l, x, r) = xs.split1(pred, mappendVal(accVpr, ml))
          (deepR(pr, ml, l), x, deepL(r, mr, sf))
        } else {
          val (l, x, r) = sf.split1(pred, accVm)
          (deepR(pr, m, l), x, r.cata(_.toTree, empty))
        }
      }
    }
  )

  def isEmpty: Boolean = fold(v => true, (v, x) => false, (v, pr, m, sf) => false)

  def viewl: ViewL[({type λ[α]=FingerTree[V, α]})#λ, A] =
    fold(
      v => EmptyL[({type λ[α]=FingerTree[V, α]})#λ, A],
      (v, x) => OnL[({type λ[α]=FingerTree[V, α]})#λ, A](x, empty[V, A]),
      (v, pr, m, sf) =>
        pr match {
          case One(v, x) => OnL[({type λ[α]=FingerTree[V, α]})#λ, A](x, rotL(m, sf))
          case _ => OnL[({type λ[α]=FingerTree[V, α]})#λ, A](pr.lhead, deep(pr.ltail, m, sf))
        })

  def viewr: ViewR[({type λ[α]=FingerTree[V, α]})#λ, A] =
    fold(
      v => EmptyR[({type λ[α]=FingerTree[V, α]})#λ, A],
      (v, x) => OnR[({type λ[α]=FingerTree[V, α]})#λ, A](empty[V, A], x),
      (v, pr, m, sf) =>
        sf match {
          case One(v, x) => OnR[({type λ[α]=FingerTree[V, α]})#λ, A](rotR(pr, m), x)
          case _ => OnR[({type λ[α]=FingerTree[V, α]})#λ, A](deep(pr, m, sf.rtail), sf.rhead)
        })

  /**
   * Selects the first element in the tree.
   *
   * @throws if the tree is empty
   */
  def head: A = viewl.head

  /**
   * Selects the last element in the tree.
   *
   * @throws if the tree is empty
   */
  def last: A = viewr.last

  /**
   * Selects a subtree containing all elements except the first
   *
   * @throws if the tree is empty
   */
  def tail: FingerTree[V, A] = viewl.tail

  /**
   * Selects a subtree containing all elements except the last
   *
   * @throws if the tree is empty
   */
  def init: FingerTree[V, A] = viewr.init

  /** Maps the given function across the tree, annotating nodes in the resulting tree according to the provided `Reducer`. */
  def map[B, V2](f: A => B)(implicit m: Reducer[B, V2]): FingerTree[V2, B] = {
    implicit val nm = nodeMeasure[B, V2]
    fold(
      v => empty,
      (v, x) => single(f(x)),
      (v, pr, mt, sf) => deep(pr map f, mt.map(x => x.map(f)), sf map f))
  }

  /**
   * Like traverse, but with a more constraint type: we need the additional measure to construct the new tree.
   */
  def traverseTree[F[_], V2, B](f: A => F[B])(implicit ms: Reducer[B, V2], F: Applicative[F]): F[FingerTree[V2, B]] = {
    def mkDeep(pr: Finger[V2, B])(m: FingerTree[V2, Node[V2, B]])(sf: Finger[V2, B]): FingerTree[V2, B] = deep(pr, m, sf)
    fold(_ => F.pure(FingerTree.empty[V2, B]),
         (v, a) => F.map(f(a))(a => single(ms.unit(a), a)),
         (v, pr, m, sf) => {
           //F.ap(traverseFinger(sf)(f))(F.ap(m.traverseTree(n => traverseNode(n)(f)))(F.map(traverseFinger(pr)(f))(pr => mkDeep(pr)_)))
           //the implementation below seems most efficient. The straightforward implementation using F.map3 leads to an explosion of traverseTree calls
           val fmap2 = F(traverseFinger(pr)(f), m.traverseTree(n => traverseNode(n)(f)))((a,b) => mkDeep(a)(b)_)
           F.ap(traverseFinger(sf)(f))(fmap2)
        })
  }

  private def traverseNode[F[_], V2, B](node: Node[V, A])(f: A => F[B])(implicit ms: Reducer[B, V2], F: Applicative[F]): F[Node[V2, B]] = {
    def mkNode(x: B)(y: B)(z: B): Node[V2, B] = node3(x, y, z)
    node.fold((v, a, b) => F(f(a), f(b))((x, y) => node2(x, y)),
        (v, a, b, c) =>  {
          F.ap(f(c))(F.ap(f(b))(F.map(f(a))(x => mkNode(x)_)))
        }
    )
  }

  private def traverseFinger[F[_], A, B, V2](digit: Finger[V, A])(f: A => F[B])(implicit ms: Reducer[B, V2], F: Applicative[F]): F[Finger[V2, B]] = {
    def mkTwo(x: B)(y: B): Finger[V2, B] = two(x, y)
    def mkThree(x: B)(y: B)(z: B): Finger[V2, B] = three(x, y, z)
    def mkFour(w: B)(x: B)(y: B)(z: B): Finger[V2, B] = four(w, x, y, z)
    digit match {
      case One(v, a) => F.map(f(a))(x => one(x))
      case Two(v, a, b) => F.ap(f(b))(F.map(f(a))(x => mkTwo(x)_))
      case Three(v, a, b, c) => F.ap(f(c))(F.ap(f(b))(F.map(f(a))(x => mkThree(x)_)))
      case Four(v, a, b, c, d) => F.ap(f(d))(F.ap(f(c))(F.ap(f(b))(F.map(f(a))(x => mkFour(x)_))))
    }
  }

  /** Execute the provided side effect for each element in the tree. */
  def foreach(f: A => Unit) {
    fold(
      _ => {},
      (_, x) => { f(x) },
      (_, pr, m, sf) => { pr.foreach(f); m.foreach(_.foreach(f)); sf.foreach(f) }
    )}

  /** An iterator that visits each element in the tree. */
  def iterator: Iterator[A] = fold(
    _ => Iterator.empty,
    (_, x) => Iterator.single(x),
    (_, pr, m, sf) => pr.iterator ++ m.iterator.flatMap(_.iterator) ++ sf.iterator)

  /** An iterator that visits each element in the tree in reverse order. */
  def reverseIterator: Iterator[A] = fold(
    _ => Iterator.empty,
    (_, x) => Iterator.single(x),
    (_, pr, m, sf) => sf.reverseIterator ++ m.reverseIterator.flatMap(_.reverseIterator) ++ pr.reverseIterator)



  import scala.collection.immutable.Stream
  import scala.collection.immutable.Stream._

  /** Convert the leaves of the tree to a `scala.Stream` */
  def toStream: Stream[A] = map(x => x)(Reducer.StreamReducer[A]).measure

  /** Convert the leaves of the tree to a `scala.List` */
  def toList: List[A] = toStream.toList

  /** Convert the tree to a `String`. Unsafe: this uses `Any#toString` for types `V` and `A` */
  override def toString = {
    import syntax.show._
    def showA[A] = new Show[A] {
      override def shows(a: A) = a.toString
    }
    implicit val v = showA[V]
    implicit val a = showA[A]
    this.shows
  }
}

class FingerTreeIntPlus[A](val value: FingerTree[Int, A]) {
  // A placeholder for a FingerTree specialized to the (Int, +) monoid
  // Will need to see how much it helps performance
}

trait FingerTreeInstances {
  import FingerTree._

  implicit def viewLFunctor[S[_]](implicit s: Functor[S]): Functor[({type λ[α]=ViewL[S, α]})#λ] = new Functor[({type λ[α]=ViewL[S, α]})#λ] {
    def map[A, B](t: ViewL[S, A])(f: A => B): ViewL[S, B] =
      t.fold(EmptyL[S, B], (x, xs) => OnL(f(x), s.map(xs)(f))) //TODO define syntax for &: and :&
  }

  implicit def viewRFunctor[S[_]](implicit s: Functor[S]): Functor[({type λ[α]=ViewR[S, α]})#λ] = new Functor[({type λ[α]=ViewR[S, α]})#λ] {
    def map[A, B](t: ViewR[S, A])(f: A => B): ViewR[S, B] =
      t.fold(EmptyR[S, B], (xs, x) => OnR(s.map(xs)(f), f(x)))
  }

  implicit def fingerFoldable[V] = new Foldable[({type l[a]=Finger[V, a]})#l] with Foldable.FromFoldMap[({type l[a]=Finger[V, a]})#l] {
    override def foldMap[A, M: Monoid](v: Finger[V, A])(f: A => M) = v.foldMap(f)
  }

  implicit def fingerMeasure[A, V](implicit m: Reducer[A, V]): Reducer[Finger[V, A], V] = {
    implicit val vm = m.monoid
    UnitReducer((a: Finger[V, A]) => a.measure)
  }

  implicit def nodeMeasure[A, V](implicit m: Reducer[A, V]): Reducer[Node[V, A], V] = {
    implicit val vm = m.monoid
    UnitReducer((a: Node[V, A]) => a fold (
            (v, _, _) => v,
            (v, _, _, _) => v))
  }

  implicit def fingerTreeMeasure[A, V](implicit m: Reducer[A, V]): Reducer[FingerTree[V, A], V] = {
    implicit val vm = m.monoid
    UnitReducer((a: FingerTree[V, A]) => a.fold(v => v, (v, x) => v, (v, x, y, z) => v))
  }

  implicit def nodeFoldable[V] = new Foldable[({type l[a]=Node[V, a]})#l] {
    def foldMap[A, M: Monoid](t: Node[V, A])(f: A => M): M = t foldMap f
    def foldRight[A, B](v: Node[V, A], z: => B)(f: (A, => B) => B): B =
       foldMap(v)((a: A) => (Endo.endo(f.curried(a)(_: B)))) apply z
  }

  implicit def fingerTreeFoldable[V]: Foldable[({type l[a]=FingerTree[V, a]})#l] = new Foldable[({type l[a]=FingerTree[V, a]})#l] {
    override def foldLeft[A, B](t: FingerTree[V, A], b: B)(f: (B, A) => B) = t.foldLeft(b)(f)

    def foldMap[A, M: Monoid](t: FingerTree[V, A])(f: A => M): M = t foldMap(f)

    override def foldRight[A, B](t: FingerTree[V, A], z: => B)(f: (A, => B) => B) = t.foldRight(z)(f)
  }

  implicit def fingerTreeSemigroup[V, A](implicit m: Reducer[A, V]): Semigroup[FingerTree[V, A]]= new Semigroup[FingerTree[V, A]] {
    def append(f1: FingerTree[V, A], f2: => FingerTree[V, A]) = f1 <++> f2
  }

  implicit def fingerTreeShow[V, A](implicit V: Show[V], A: Show[A]): Show[FingerTree[V,A]] = new Show[FingerTree[V,A]] {
    import syntax.show._
    import std.list._
    val AS = Show[List[A]]
    import Cord._
    override def show(t: FingerTree[V,A]) = t.fold(
      empty = v => Cord(V.show(v), " []"),
      single = (v, x) => Cord(V.show(v), " [", A.show(x), "]"),
      deep = (v, pf, m, sf) => Cord(V.show(v), " [", AS.show(pf.toList), ", ?, ", AS.show(sf.toList), "]")
    )
  }

  implicit def fingerTreeEqual[V, A : Equal]: Equal[FingerTree[V, A]] = new Equal[FingerTree[V, A]] {
    import std.stream._
    def equal(x: FingerTree[V, A], y: FingerTree[V, A]) =
      Equal[Stream[A]].equal(x.toStream, y.toStream)
  }

  import FingerTree.ftip2ft
  import Rope.Rope

  implicit def ropeLength: Length[Rope] = new Length[Rope] {
    def length[A](a: Rope[A]) = a.self.measure
  }
}

trait FingerTreeFunctions {

  def Node2[V, A](v: V, a1: => A, a2: => A)(implicit r: Reducer[A, V]) = new Node[V, A] {
    def fold[B](two: (V, => A, => A) => B, three: (V, => A, => A, => A) => B) =
      two(v, a1, a2)
    val measure = v
  }

  def Node3[V, A](v: V, a1: => A, a2: => A, a3: => A)(implicit r: Reducer[A, V]) = new Node[V, A] {
    def fold[B](two: (V, => A, => A) => B, three: (V, => A, => A, => A) => B) =
      three(v, a1, a2, a3)
    val measure = v
  }

  def EmptyR[S[_], A]: ViewR[S, A] = new ViewR[S, A] {
    def fold[B](b: => B, f: (=> S[A], => A) => B) = b
  }

  def OnR[S[_], A](sa: => S[A], a: => A): ViewR[S, A] = new ViewR[S, A] {
    def fold[B](b: => B, f: (=> S[A], => A) => B) = f(sa, a)
  }

  def EmptyL[S[_], A]: ViewL[S, A] = new ViewL[S, A] {
    def fold[B](b: => B, f: (=> A, => S[A]) => B) = b
  }

  def OnL[S[_], A](a: => A, sa: => S[A]): ViewL[S, A] = new ViewL[S, A] {
    def fold[B](b: => B, f: (=> A, => S[A]) => B) = f(a, sa)
  }

  def one[V, A](a: => A)(implicit measure: Reducer[A, V]) =
    One(a.unit[V], a)

  def two[V, A](a1: => A, a2: => A)(implicit measure: Reducer[A, V]) =
    Two(a1.unit[V] snoc a2, a1, a2)

  def three[V, A](a1: => A, a2: => A, a3: => A)(implicit measure: Reducer[A, V]) =
    Three(a1.unit[V] snoc a2 snoc a3, a1, a2, a3)

  def four[V, A](a1: => A, a2: => A, a3: => A, a4: => A)(implicit measure: Reducer[A, V]) =
    Four(a1.unit[V] snoc a2 snoc a3 snoc a4, a1, a2, a3, a4)

  def node2[V, A](a: => A, b: => A)(implicit measure: Reducer[A, V]) =
    Node2[V, A](a.unit[V] snoc b, a, b)

  def node3[V, A](a: => A, b: => A, c: => A)(implicit measure: Reducer[A, V]) =
    Node3[V, A](a.unit[V] snoc b snoc c, a, b, c)

  def mappendVal[V, A](v: V, t: FingerTree[V, A])(implicit measure: Reducer[A, V]) = {
    t.fold(x => v, (x, y) => v snoc t, (x, p, m, s) => v snoc t)
  }

  def empty[V, A](implicit ms: Reducer[A, V]) = new FingerTree[V, A] {
    def fold[B](b: V => B, s: (V, A) => B, d: (V, Finger[V, A], => FingerTree[V, Node[V, A]], Finger[V, A]) => B): B = b(ms.monoid.zero)
  }

  def single[V, A](a: => A)(implicit ms: Reducer[A, V]): FingerTree[V, A] = single(a.unit[V], a)

  def single[V, A](v: V, a: => A)(implicit ms: Reducer[A, V]): FingerTree[V, A] = new FingerTree[V, A] {
    def fold[B](b: V => B, s: (V, A) => B, d: (V, Finger[V, A], => FingerTree[V, Node[V, A]], Finger[V, A]) => B): B = s(v, a)
  }

  def deep[V, A](pr: Finger[V, A], m: => FingerTree[V, Node[V, A]], sf: Finger[V, A])
             (implicit ms: Reducer[A, V]): FingerTree[V, A] = {
    deep(mappendVal(pr.unit[V], m) snoc sf, pr, m, sf)
  }

  def deep[V, A](v: V, pr: Finger[V, A], m: => FingerTree[V, Node[V, A]], sf: Finger[V, A])
             (implicit ms: Reducer[A, V]): FingerTree[V, A] =
    new FingerTree[V, A] {
      implicit val nMeasure = nodeMeasure[A, V]
      lazy val mz = m
      def fold[B](b: V => B, f: (V, A) => B, d: (V, Finger[V, A], => FingerTree[V, Node[V, A]], Finger[V, A]) => B): B =
        d(v, pr, mz, sf)
    }

  def deepL[V, A](mpr: Option[Finger[V, A]], m: => FingerTree[V, Node[V, A]], sf: Finger[V, A])(implicit ms: Reducer[A, V]): FingerTree[V, A] =
    mpr match {
      case None => rotL(m, sf)
      case Some(pr) => deep(pr, m, sf)
    }

  def deepR[V, A](pr: Finger[V, A], m: => FingerTree[V, Node[V, A]], msf: Option[Finger[V, A]])(implicit ms: Reducer[A, V]): FingerTree[V, A] =
    msf match {
      case None => rotR(pr, m)
      case Some(sf) => deep(pr, m, sf)
    }

  def rotL[V, A](m: FingerTree[V, Node[V, A]], sf: Finger[V, A])(implicit ms: Reducer[A, V]): FingerTree[V, A] =
    m.viewl.fold(
      sf.toTree,
      (a, mm) => deep(m.measure snoc sf, a.toDigit, mm, sf))

  def rotR[V, A](pr: Finger[V, A], m: FingerTree[V, Node[V, A]])(implicit ms: Reducer[A, V]): FingerTree[V, A] =
    m.viewr.fold(
      pr.toTree,
      (mm, a) => deep(mappendVal(pr.measure, m), pr, mm, a.toDigit))

  implicit def ft2ftip[A](ft: FingerTree[Int, A]): FingerTreeIntPlus[A] = new FingerTreeIntPlus(ft)

  implicit def ftip2ft[A](ft: FingerTreeIntPlus[A]): FingerTree[Int, A] = ft.value

  import collection.IndexedSeqLike
  import collection.immutable.IndexedSeq
  import collection.mutable.Builder
  import collection.generic.CanBuildFrom

  trait Ropes {
    import scalaz.{ImmutableArray => IA}
    import std.anyVal._

    /**
     * Ropes or 'heavyweight Strings' are an alternative to Strings.
     * In essence they are binary trees whose leaves are arrays of characters.
     * Their advantage over ordinary strings is support for efficient concatenation and substring operations,
     * which scale to long strings.
     *
     * They were first described in the paper: Ropes: an Alternative to Strings.
     * by Hans-J. Boehm , Russ Atkinson , Michael Plass
     *
     * @see [[http://citeseer.ist.psu.edu/viewdoc/download?doi=10.1.1.14.9450&rep=rep1&type=pdf]]
     */
    sealed class Rope[A : ClassManifest](val self: FingerTreeIntPlus[ImmutableArray[A]])
             extends Ops[FingerTreeIntPlus[ImmutableArray[A]]] {
      import Rope._
      implicit def sizer = UnitReducer((arr: ImmutableArray[A]) => arr.length)

      def length: Int = self.measure

      def apply(i: Int): A = {
        val split = self.split(_ > i)
        split._2.viewl.headOption.getOrElse(sys.error("Index " + i + " > " + self.measure))(i - split._1.value.measure)
      }

      /**Concatenates two Ropes. `(O lg min(r1, r2))` where `r1` and `r2` are their sizes. */
      def ++(xs: Rope[A]): Rope[A] = rope(self <++> xs.self)

      /**Appends the given chunk to the rope*/
      def ::+(chunk: ImmutableArray[A]): Rope[A] =
        if (chunk.isEmpty)
          this
        else
          rope(
            self.viewr.fold(
              single(chunk)(sizer),
              (_, last) =>
                if (last.length + chunk.length <= baseChunkLength)
                  self :-| (last ++ chunk)
                else
                  self :+ chunk
            )
          )

      /**Prepends the given chunk to this rope*/
      def +::(chunk: ImmutableArray[A]): Rope[A] =
        if (chunk.isEmpty)
          this
        else
          rope(
            self.viewl.fold(
              single(chunk)(sizer),
              (head, _) =>
                if (chunk.length + head.length <= baseChunkLength)
                  (chunk ++ head) |-: self
                else
                  chunk +: self
            )
          )

      /**Appends the given element to this rope*/
      def :+(x: A): Rope[A] = this ::+ IA.fromArray(Array(x))

      /**Prepends the given element to this rope*/
      def +:(x: A): Rope[A] = IA.fromArray(Array(x)) +:: this

      /**tail of the Rope*/
      def tail: Rope[A] = rope(self.tail)

      /**first element of the rope*/
      def init: Rope[A] = rope(self.init)
//      def map[B](f: A => B) = rope(value map f) TODO
//      def flatMap[B](f: A => Rope[B]) =
//        rope(value.foldl(empty[Int, B])((ys, x) => ys <++> f(x).value))

      // override def foreach[U](f: A => U): Unit = value.foreach(_.foreach(f))

      def iterator: Iterator[A] = self.iterator.flatMap(_.iterator)
      def reverseIterator: Iterator[A] = self.reverseIterator.flatMap(_.reverseIterator)

      // TODO override def reverse

      def chunks: Stream[ImmutableArray[A]] = self.toStream

      // protected[this] override def newBuilder: Builder[A, Rope[A]] = new RopeBuilder[A]
    }

    object Rope {
      private[Ropes] val baseChunkLength = 16
      implicit def sizer[A]: Reducer[ImmutableArray[A], Int] = UnitReducer(_.length)
      def empty[A : ClassManifest] = rope(FingerTree.empty[Int, ImmutableArray[A]])
      def fromArray[A : ClassManifest](a: Array[A]): Rope[A] =
        if (a.isEmpty) empty[A] else rope(single(IA.fromArray(a)))
      def fromString(str: String): Rope[Char] =
        if (str.isEmpty) empty[Char] else rope(single(IA.fromString(str)))
      def fromChunks[A : ClassManifest](chunks: Seq[ImmutableArray[A]]): Rope[A] =
        rope(chunks.foldLeft(FingerTree.empty[Int, ImmutableArray[A]])((tree, chunk) => if (!chunk.isEmpty) tree :+ chunk else tree))
//      def apply[A](as: A*) = fromSeq(as)
//      def fromSeq[A](as: Seq[A]) = rope(as.foldLeft(empty[Int, A](Reducer(a => 1)))((x, y) => x :+ y))

      def newBuilder[A : ClassManifest]: Builder[A, Rope[A]] = new RopeBuilder[A]

      implicit def canBuildFrom[T : ClassManifest]: CanBuildFrom[Rope[_], T, Rope[T]] =
        new CanBuildFrom[Rope[_], T, Rope[T]] {
          def apply(from: Rope[_]): Builder[T, Rope[T]] = newBuilder[T]
          def apply(): Builder[T, Rope[T]] = newBuilder[T]
        }
    }

    def rope[A : ClassManifest](v: FingerTreeIntPlus[ImmutableArray[A]]): Rope[A] = new Rope[A](v)

    sealed class WrappedRope[A : ClassManifest](val self: Rope[A])
        extends Ops[Rope[A]] with IndexedSeq[A] with IndexedSeqLike[A, WrappedRope[A]] {
      import Rope._

      def apply(i: Int): A = self(i)

      def ++(xs: WrappedRope[A]) = wrapRope(self ++ xs.self)

      // override def :+(x: A) = wrapRope(value :+ x)
      // override def +:(x: A) = wrapRope(x +: value)
      override def tail = self.tail
      override def init = self.init
//      def map[B](f: A => B) = rope(value map f)
//      def flatMap[B](f: A => Rope[B]) =
//        rope(value.foldl(empty[Int, B])((ys, x) => ys <++> f(x).value))

      // override def foreach[U](f: A => U): Unit = value.foreach(_.foreach(f))

      override def iterator: Iterator[A] = self.self.iterator.flatMap(_.iterator)

      override def reverseIterator: Iterator[A] = self.self.reverseIterator.flatMap(_.reverseIterator)

      // TODO override def reverse

      override def toStream = self.chunks.flatten

      override def length = self.length

      protected[this] override def newBuilder = new RopeBuilder[A].mapResult(wrapRope(_))
    }

    implicit def wrapRope[A : ClassManifest](rope: Rope[A]): WrappedRope[A] = new WrappedRope(rope)
    implicit def unwrapRope[A : ClassManifest](wrappedRope: WrappedRope[A]): Rope[A] = wrappedRope.self

    final class RopeBuilder[A : ClassManifest] extends Builder[A, Rope[A]] {
      import Rope._
      private var startRope: Rope[A] = Rope.empty[A]
      private var tailBuilder: Builder[A, ImmutableArray[A]] = IA.newBuilder[A]
      private var tailLength = 0

      def clear() {
        startRope = Rope.empty[A]
        tailBuilder = IA.newBuilder[A]
        tailLength = 0
      }

      def +=(elem: A) = {
        if (tailLength < baseChunkLength) {
          tailBuilder += elem
          tailLength += 1
        }
        else {
          cleanTail
          tailBuilder += elem
          tailLength = 1
        }
        this
      }

      def result = startRope ::+ tailBuilder.result

      override def sizeHint(size: Int) {
        tailBuilder.sizeHint(math.min(size - startRope.length, baseChunkLength))
      }

      // TODO fix and reinstate
//      import collection.mutable.ArrayLike
//      override def ++=(xs: TraversableOnce[A]) = {
//        xs match {
//          case xs: Rope[A] => {
//            cleanTail
//            startRope ++= xs
//          }
//          case xs: ImmutableArray[A] => {
//            cleanTail
//            startRope ::+= xs
//          }
//          case xs: ArrayLike[A, _] => {
//            cleanTail
//            tailBuilder ++= xs
//          }
//          case _ =>  super.++=(xs)
//        }
//        this
//      }
//    }

      private def cleanTail {
        startRope ::+= tailBuilder.result
        tailBuilder.clear()
      }
    }

    sealed class RopeCharW(val self: Rope[Char]) extends Ops[Rope[Char]] {
      def asString = {
        val stringBuilder = new StringBuilder(self.length)
        appendTo(stringBuilder)
        stringBuilder.toString
      }

      def appendTo(stringBuilder: StringBuilder) {
        self.chunks.foreach(ia => stringBuilder.append(ia.asString))
      }
    }

    implicit def wrapRopeChar(rope: Rope[Char]): RopeCharW = new RopeCharW(rope)
  }
}

object Rope extends Ropes

object FingerTree extends FingerTreeInstances with FingerTreeFunctions

/** Indexed sequences, based on [[scalaz.FingerTree]]
 *
 * The measure is the count of the preceding elements, provided by `UnitReducer((e: Int) => 1)`.
 */
sealed trait IndSeq[A] extends Ops[FingerTree[Int, A]] {

  import std.anyVal._
  import IndSeq.indSeq

  implicit def sizer[A] = UnitReducer((a: A) => 1)
  def apply(i: Int): A =
    self.split(_ > i)._2.viewl.headOption.getOrElse(sys.error("Index " + i + " > " + self.measure))
  def replace(i: Int, a: => A): IndSeq[A] = {
    val (l, r) = self.split(_ > i)
    indSeq(l <++> (a |-: r))
  }
  def split(i: Int): (IndSeq[A], IndSeq[A]) = {
    val (l, r) = self.split(_ > i)
    (indSeq(l), indSeq(r))
  }
  def ++(xs: IndSeq[A]): IndSeq[A] = indSeq(self <++> xs.self)
  def :+(x: => A): IndSeq[A] = indSeq(self :+ x)
  def +:(x: => A): IndSeq[A] = indSeq(x +: self)
  def tail: IndSeq[A] = indSeq(self.tail)
  def init: IndSeq[A] = indSeq(self.init)
  def drop(n: Int): IndSeq[A] = split(n)._2
  def take(n: Int): IndSeq[A] = split(n)._1
  def map[B](f: A => B): IndSeq[B] = indSeq(self map f)

  import FingerTree.fingerTreeFoldable

  def flatMap[B](f: A => IndSeq[B]): IndSeq[B] = indSeq(fingerTreeFoldable.foldLeft(self, empty[Int, B])((ys, x) => ys <++> f(x).self))
}

object IndSeq {
  private def indSeq[A](v: FingerTree[Int, A]) = new IndSeq[A] {
    val self = v
  }

  import std.anyVal._

  def apply[A](as: A*) = fromSeq(as)
  def fromSeq[A](as: Seq[A]) = indSeq(as.foldLeft(empty[Int, A](UnitReducer(a => 1)))((x, y) => x :+ y))
}


/** Ordered sequences, based on [[scalaz.FingerTree]]
 *
 *  `a` has a higher priority than `b` if `Order[A].greaterThan(a, b)`.
 *
 * `insert` and `++` maintains the ordering.
 *
 * The measure is calculated with a `Monoid[Option[A] @@ Last]`, whose `append`
 * operation favours the first argument. Accordingly, the measuer of a node is the
 * item with the highest priority contained recursively below that node.
 */
sealed trait OrdSeq[A] extends Ops[FingerTree[LastOption[A], A]] {
  import syntax.arrow._
  import std.function._
  import syntax.order._
  import std.option._

  implicit val ord: Order[A]

  /**
   * @return (higher, lowerOrEqual) The sub-sequences that contain elements of higher and of lower-than-or-equal
   *                                priority than `a`, and of lower or equal priority respectively.
   */
  def partition(a: A): (OrdSeq[A], OrdSeq[A]) =
  function1Instance.product(OrdSeq.ordSeq[A](_: FingerTree[LastOption[A], A]))(self.split(_ gte Tags.Last(some(a))))

  /** Insert `a` at a the first point that all elements to the left are of higher priority */
  def insert(a: A): OrdSeq[A] = partition(a) match {
    case (l, r) => OrdSeq.ordSeq(l <++> (a +: r))
  }

  /** Append `xs` to this sequence, reordering elements to  */
  def ++(xs: OrdSeq[A]): OrdSeq[A] = xs.self.toList.foldLeft(this)(_ insert _)
}

object OrdSeq {
  private def ordSeq[A: Order](t: FingerTree[LastOption[A], A]): OrdSeq[A] = new OrdSeq[A] {
    val self = t
    val ord = Order[A]
  }

  implicit def unwrap[A](t: OrdSeq[A]): FingerTree[LastOption[A], A] = t.self

  def apply[A: Order](as: A*): OrdSeq[A] = {
    val z: OrdSeq[A] = {
      val keyer: Reducer[A, LastOption[A]] = UnitReducer((a: A) => Tags.Last(some(a)))
      ordSeq(empty[LastOption[A], A](keyer))
    }
    as.foldLeft(z)((x, y) => x insert y)
  }
}



