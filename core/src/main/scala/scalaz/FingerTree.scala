package scalaz

import scalaz.Scalaz._

sealed abstract class ViewL[S[_], A] {
  def fold[B](b: => B, f: (=> A, => S[A]) => B): B
  def head: A = fold(error("Head on empty view"), (a, sa) => a)
  def tail: S[A] = fold(error("Tail on empty view"), (a, sa) => sa)
}

sealed abstract class ViewR[S[_], A] {
  def fold[B](b: => B, f: (=> S[A], => A) => B): B
  def last: A = fold(error("Head on empty view"), (sa, a) => a)
  def init: S[A] = fold(error("Tail on empty view"), (sa, a) => sa)
}

import FingerTree._

sealed abstract class Finger[V, A] {
  def foldMap[B](f: A => B)(implicit m: Semigroup[B]): B

  def +:(a: A): Finger[V, A]

  def :+(a: A): Finger[V, A]

  def |-:(a: A): Finger[V, A]

  def :-|(a: A): Finger[V, A]

  def lhead: A

  def ltail: Finger[V, A]

  def rhead: A

  def rtail: Finger[V, A]

  def toTree: FingerTree[V, A]

  def map[B, V2](f: A => B)(implicit m: Reducer[B, V2]): Finger[V2, B]

  def measure: V

  def toList = map(x => x)(ListReducer[A]).measure

  private[scalaz] def split1(pred: V => Boolean, accV: V): (Option[Finger[V, A]], A, Option[Finger[V, A]])
}
case class One[V, A](v: V, a1: A)(implicit r: Reducer[A, V]) extends Finger[V, A] {
  def foldMap[B](f: A => B)(implicit m: Semigroup[B]) = f(a1)

  def +:(a: A) = Two(a cons v, a, a1)

  def :+(a: A) = Two(v snoc a, a1, a)

  def |-:(a: A) = one(a)

  def :-|(a: A) = one(a)

  def lhead = a1

  def ltail = error("Tail on the digit One")

  def rhead = a1

  def rtail = error("Tail on the digit One")

  def toTree = single(a1)

  def map[B, V2](f: A => B)(implicit r: Reducer[B, V2]) = one(f(a1))

  val measure = v

  private[scalaz] def split1(pred: V => Boolean, accV: V) = (None, a1, None)
}
case class Two[V, A](v: V, a1: A, a2: A)(implicit r: Reducer[A, V]) extends Finger[V, A] {
  def foldMap[B](f: A => B)(implicit m: Semigroup[B]) = f(a1) |+| f(a2)

  def +:(a: A) = Three(a cons v, a, a1, a2)

  def :+(a: A) = Three(v snoc a, a1, a2, a)

  def |-:(a: A) = two(a, a2)

  def :-|(a: A) = two(a1, a)

  def lhead = a1

  def ltail = one(a2)

  def rhead = a2

  def rtail = one(a1)

  def toTree = {
    deep(v, one(a1), empty[V, Node[V, A]], one(a2))
  }

  def map[B, V2](f: A => B)(implicit r: Reducer[B, V2]) = two(f(a1), f(a2))

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

  def +:(a: A) = Four(a cons v, a, a1, a2, a3)

  def :+(a: A) = Four(v snoc a, a1, a2, a3, a)

  def |-:(a: A) = three(a, a2, a3)

  def :-|(a: A) = three(a1, a2, a)

  def lhead = a1

  def ltail = two(a2, a3)

  def rhead = a3

  def rtail = two(a1, a2)

  def toTree = {
    deep(v, two(a1, a2), empty[V, Node[V, A]], one(a3))
  }

  def map[B, V2](f: A => B)(implicit r: Reducer[B, V2]) = three(f(a1), f(a2), f(a3))

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

  def +:(a: A) = error("Digit overflow")

  def :+(a: A) = error("Digit overflow")

  def |-:(a: A) = four(a, a2, a3, a4)

  def :-|(a: A) = four(a1, a2, a3, a)

  def lhead = a1

  def ltail = three(a2, a3, a4)

  def rhead = a4

  def rtail = three(a1, a2, a3)

  def toTree = {
    deep(v, two(a1, a2), empty[V, Node[V, A]], two(a3, a4))
  }

  def map[B, V2](f: A => B)(implicit r: Reducer[B, V2]) = four(f(a1), f(a2), f(a3), f(a4))

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
  def fold[B](two: (V, A, A) => B, three: (V, A, A, A) => B): B

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

sealed abstract class FingerTree[V, A](implicit measurer: Reducer[A, V]) {
  def measure = this.unit[V]

  def foldMap[B](f: A => B)(implicit s: Monoid[B]): B =
    fold(v => s.zero, (v, x) => f(x), (v, pr, m, sf) => pr.foldMap(f) |+| m.foldMap(x => x.foldMap(f)) |+| sf.foldMap(f))

  def fold[B](empty: V => B, single: (V, A) => B, deep: (V, Finger[V, A], => FingerTree[V, Node[V, A]], Finger[V, A]) => B): B

  def +:(a: A): FingerTree[V, A] = {
    implicit val nm = NodeMeasure[A, V]
    fold(v => single(a cons v, a), (v, b) => deep(a cons v, one(a), empty[V, Node[V, A]], one(b)), (v, pr, m, sf) =>
      pr match {
        case Four(vf, b, c, d, e) => deep(a cons v, two(a, b), node3(c, d, e) +: m, sf)
        case _ => deep(a cons v, a +: pr, m, sf)
      })
  }

  def :+(a: A): FingerTree[V, A] = {
    implicit val nm = NodeMeasure[A, V]
    fold(v => single(v snoc a, a), (v, b) => deep(v snoc a, one(b), empty[V, Node[V, A]], one(a)), (v, pr, m, sf) =>
      sf match {
        case Four(vf, b, c, d, e) => deep(v snoc a, pr, (m :+ node3(b, c, d)), two(e, a))
        case _ => deep(v snoc a, pr, m, sf :+ a)
      })
  }

  def |-:(a: A): FingerTree[V, A] = {
    fold(
      v => error("Replacing first element of an empty FingerTree"),
      (v, b) => single(a),
      (v, pr, m, sf) => deep(a |-: pr, m, sf))
  }

  def :-|(a: A): FingerTree[V, A] = {
    fold(
      v => error("Replacing last element of an empty FingerTree"),
      (v, b) => single(a),
      (v, pr, m, sf) => deep(pr, m, sf :-| a))
  }

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

  private implicit def sg: Semigroup[V] = measurer.monoid

  def add1(n: A, right: ATree): ATree = fold(
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

  def add2(n1: A, n2: A, right: ATree): ATree = fold(
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

  def add3(n1: A, n2: A, n3: A, right: ATree): ATree = fold(
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

  def add4(n1: A, n2: A, n3: A, n4: A, right: ATree): ATree = fold(
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

  def addDigits0(m1: NodeTree, dig1: AFinger, dig2: AFinger, m2: NodeTree): NodeTree = dig1 match {
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

  def addDigits1(m1: NodeTree, d1: AFinger, x: A, d2: AFinger, m2: NodeTree): NodeTree = d1 match {
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

  def addDigits2(m1: NodeTree, d1: AFinger, x: A, y: A, d2: AFinger, m2: NodeTree): NodeTree = d1 match {
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

  def addDigits3(m1: NodeTree, d1: AFinger, x: A, y: A, z: A, d2: AFinger, m2: NodeTree): NodeTree = d1 match {
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

  def addDigits4(m1: NodeTree, d1: AFinger, x: A, y: A, z: A, w: A, d2: AFinger, m2: NodeTree): NodeTree = d1 match {
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

  def split(pred: V => Boolean): (FingerTree[V, A], FingerTree[V, A]) =
    if (!isEmpty && pred(measure)) {
      val (l, x, r) = split1(pred)
      (l, x +: r)
    }
    else
      (this, empty)

  def split1(pred: V => Boolean): (FingerTree[V, A], A, FingerTree[V, A]) = split1(pred, measurer.monoid.zero)

  private def split1(pred: V => Boolean, accV: V): (FingerTree[V, A], A, FingerTree[V, A]) = fold(
    v => error("Splitting an empty FingerTree"), // we can never get here
    (v, x) => (empty, x, empty),
    (v, pr, m, sf) => {
      val accVpr = accV snoc pr
      if (pred(accVpr)) {
        val (l, x, r) = pr.split1(pred, accV)
        (l.cata(_.toTree, empty), x, deepL(r, m, sf))
      }
      else {
        val accVm = mappendVal(accVpr, m)
        if (pred(accVm)) {
          val (ml, xs, mr) = m.split1(pred, accVpr)
          val (l, x, r) = xs.split1(pred, mappendVal(accVpr, ml))
          (deepR(pr, ml, l), x, deepL(r, mr, sf))
        }
        else {
          val (l, x, r) = sf.split1(pred, accVm)
          (deepR(pr, m, l), x, r.cata(_.toTree, empty))
        }
      }
    }
  )

  def isEmpty = fold(v => true, (v, x) => false, (v, pr, m, sf) => false)

  def viewl: ViewL[PartialApply1Of2[FingerTree, V]#Apply, A] =
    fold(
      v => EmptyL[PartialApply1Of2[FingerTree, V]#Apply, A],
      (v, x) => OnL[PartialApply1Of2[FingerTree, V]#Apply, A](x, empty[V, A]),
      (v, pr, m, sf) =>
        pr match {
          case One(v, x) => OnL[PartialApply1Of2[FingerTree, V]#Apply, A](x, rotL(m, sf))
          case _ => OnL[PartialApply1Of2[FingerTree, V]#Apply, A](pr.lhead, deep(pr.ltail, m, sf))
        })

  def viewr: ViewR[PartialApply1Of2[FingerTree, V]#Apply, A] =
    fold(
      v => EmptyR[PartialApply1Of2[FingerTree, V]#Apply, A],
      (v, x) => OnR[PartialApply1Of2[FingerTree, V]#Apply, A](empty[V, A], x),
      (v, pr, m, sf) =>
        sf match {
          case One(v, x) => OnR[PartialApply1Of2[FingerTree, V]#Apply, A](rotR(pr, m), x)
          case _ => OnR[PartialApply1Of2[FingerTree, V]#Apply, A](deep(pr, m, sf.rtail), sf.rhead)
        })

  def head = viewl.head

  def last = viewr.last

  def tail = viewl.tail

  def init = viewr.init

  def map[B, V2](f: A => B)(implicit m: Reducer[B, V2]): FingerTree[V2, B] = {
    implicit val nm = NodeMeasure[B, V2]
    fold(
      v => empty,
      (v, x) => single(f(x)),
      (v, pr, mt, sf) => deep(pr map f, mt.map(x => x.map(f)), sf map f))
  }

  import scala.collection.immutable.Stream
  import scala.collection.immutable.Stream._

  def toStream: Stream[A] = map(x => x)(StreamReducer[A]).measure 
  def toList: List[A] = toStream.toList

  import FingerTree._

  override def toString = {
    implicit val v = showA[V]
    implicit val a = showA[A]
    this.shows
  }
}

object FingerTree {
  implicit def FingerTreeShow[V: Show, A: Show]: Show[FingerTree[V,A]] = shows((t: FingerTree[V,A]) => t.fold(
    empty = v => v + " []",
    single = (v, x) => v + " [" + x.shows + "]",
    deep = (v, pf, m, sf) => v + " [" + pf.toList.shows + ", ?, " + sf.toList.shows + "]"
  ))

  def Node2[V, A](v: V, a1: A, a2: A)(implicit r: Reducer[A, V]) = new Node[V, A] {
    def fold[B](two: (V, A, A) => B, three: (V, A, A, A) => B) =
      two(v, a1, a2)
    val measure = v
  }

  def Node3[V, A](v: V, a1: A, a2: A, a3: A)(implicit r: Reducer[A, V]) = new Node[V, A] {
    def fold[B](two: (V, A, A) => B, three: (V, A, A, A) => B) =
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

  implicit def FingerFoldable[V] = new Foldable[PartialApply1Of2[Finger, V]#Apply] {
    override def foldMap[A, M: Monoid](v: Finger[V, A], f: A => M) = v.foldMap(f)
  }

  implicit def FingerMeasure[A, V](implicit m: Reducer[A, V]): Reducer[Finger[V, A], V] = {
    implicit val vm = m.monoid
    Reducer((a: Finger[V, A]) => a.measure)
  }

  implicit def NodeMeasure[A, V](implicit m: Reducer[A, V]): Reducer[Node[V, A], V] = {
    implicit val vm = m.monoid
    Reducer((a: Node[V, A]) => a fold (
            (v, _, _) => v,
            (v, _, _, _) => v))
  }

  implicit def FingerTreeMeasure[A, V](implicit m: Reducer[A, V]): Reducer[FingerTree[V, A], V] = {
    implicit val vm = m.monoid
    Reducer((a: FingerTree[V, A]) => a.fold(v => v, (v, x) => v, (v, x, y, z) => v))
  }

  def one[V, A](a: A)(implicit measure: Reducer[A, V]) = 
    One(a.unit[V], a)

  def two[V, A](a1: A, a2: A)(implicit measure: Reducer[A, V]) = 
    Two(a1.unit[V] snoc a2, a1, a2)

  def three[V, A](a1: A, a2: A, a3: A)(implicit measure: Reducer[A, V]) = 
    Three(a1.unit[V] snoc a2 snoc a3, a1, a2, a3)

  def four[V, A](a1: A, a2: A, a3: A, a4: A)(implicit measure: Reducer[A, V]) = 
    Four(a1.unit[V] snoc a2 snoc a3 snoc a4, a1, a2, a3, a4)

  def node2[V, A](a: A, b: A)(implicit measure: Reducer[A, V]) =
    Node2[V, A](a.unit[V] snoc b, a, b)

  def node3[V, A](a: A, b: A, c: A)(implicit measure: Reducer[A, V]) =
    Node3[V, A](a.unit[V] snoc b snoc c, a, b, c)

  private def mappendVal[V, A](v: V, t: FingerTree[V, A])(implicit measure: Reducer[A, V]) = {
    t.fold(x => v, (x, y) => v snoc t, (x, p, m, s) => v snoc t)
  }

  def empty[V, A](implicit ms: Reducer[A, V]) = new FingerTree[V, A] {
    def fold[B](b: V => B, s: (V, A) => B, d: (V, Finger[V, A], => FingerTree[V, Node[V, A]], Finger[V, A]) => B): B = b(ms.monoid.zero)
  }

  def single[V, A](a: A)(implicit ms: Reducer[A, V]): FingerTree[V, A] = single(a.unit[V], a)

  def single[V, A](v: V, a: A)(implicit ms: Reducer[A, V]): FingerTree[V, A] = new FingerTree[V, A] {
    def fold[B](b: V => B, s: (V, A) => B, d: (V, Finger[V, A], => FingerTree[V, Node[V, A]], Finger[V, A]) => B): B = s(v, a)
  }

  def deep[V, A](pr: Finger[V, A], m: => FingerTree[V, Node[V, A]], sf: Finger[V, A])
             (implicit ms: Reducer[A, V]): FingerTree[V, A] = {
    deep(mappendVal(pr.unit[V], m) snoc sf, pr, m, sf)
  }

  def deep[V, A](v: V, pr: Finger[V, A], m: => FingerTree[V, Node[V, A]], sf: Finger[V, A])
             (implicit ms: Reducer[A, V]): FingerTree[V, A] = 
    new FingerTree[V, A] {
      implicit val nodeMeasure = NodeMeasure[A, V]
      lazy val mz = m
      def fold[B](b: V => B, f: (V, A) => B, d: (V, Finger[V, A], => FingerTree[V, Node[V, A]], Finger[V, A]) => B): B =
        d(v, pr, mz, sf)
    }

  private def deepL[V, A](mpr: Option[Finger[V, A]], m: FingerTree[V, Node[V, A]], sf: Finger[V, A])(implicit ms: Reducer[A, V]): FingerTree[V, A] =
    mpr match {
      case None => rotL(m, sf)
      case Some(pr) => deep(pr, m, sf)
    }

  private def deepR[V, A](pr: Finger[V, A], m: FingerTree[V, Node[V, A]], msf: Option[Finger[V, A]])(implicit ms: Reducer[A, V]): FingerTree[V, A] =
    msf match {
      case None => rotR(pr, m)
      case Some(sf) => deep(pr, m, sf)
    }

  private def rotL[V, A](m: FingerTree[V, Node[V, A]], sf: Finger[V, A])(implicit ms: Reducer[A, V]): FingerTree[V, A] =
    m.viewl.fold(
      sf.toTree,
      (a, mm) => deep(m.measure snoc sf, a.toDigit, mm, sf))

  private def rotR[V, A](pr: Finger[V, A], m: FingerTree[V, Node[V, A]])(implicit ms: Reducer[A, V]): FingerTree[V, A] =
    m.viewr.fold(
      pr.toTree,
      (mm, a) => deep(mappendVal(pr.measure, m), pr, mm, a.toDigit))
}
