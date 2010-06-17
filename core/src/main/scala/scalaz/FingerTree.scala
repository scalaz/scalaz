package scalaz

import scalaz.Scalaz._

sealed abstract class ViewL[S[_], A] {
  def fold[B](b: => B, f: (A, S[A]) => B): B
}

sealed abstract class ViewR[S[_], A] {
  def fold[B](b: => B, f: (S[A], A) => B): B
}

import FingerTree._

sealed abstract class Finger[V, A] {
  def foldMap[B](f: A => B)(implicit m: Semigroup[B]): B

  def <+:(a: A): Finger[V, A]

  def :+>(a: A): Finger[V, A]

  def lhead: A

  def ltail: Finger[V, A]

  def rhead: A

  def rtail: Finger[V, A]

  def toTree[V](implicit m: Reducer[A, V]): FingerTree[V, A]

  def map[B, V2](f: A => B)(implicit m: Reducer[B, V2]): Finger[V2, B]

  def measure: V
}
case class One[V, A](v: V, a1: A)(implicit r: Reducer[A, V]) extends Finger[V, A] {
  def foldMap[B](f: A => B)(implicit m: Semigroup[B]) = f(a1)

  def <+:(a: A) = Two(a cons v, a, a1)

  def :+>(a: A) = Two(v snoc a, a1, a)

  def lhead = a1

  def ltail = error("Tail on the digit One")

  def rhead = a1

  def rtail = error("Tail on the digit One")

  def toTree[V](implicit m: Reducer[A, V]) = single(a1)

  def map[B, V2](f: A => B)(implicit r: Reducer[B, V2]) = one(f(a1))

  val measure = v
}
case class Two[V, A](v: V, a1: A, a2: A)(implicit r: Reducer[A, V]) extends Finger[V, A] {
  def foldMap[B](f: A => B)(implicit m: Semigroup[B]) = f(a1) |+| f(a2)

  def <+:(a: A) = Three(a cons v, a, a1, a2)

  def :+>(a: A) = Three(v snoc a, a1, a2, a)

  def lhead = a1

  def ltail = one(a2)

  def rhead = a2

  def rtail = one(a1)

  def toTree[V](implicit m: Reducer[A, V]) = {
    deep(one(a1), empty[V, Node[V, A]], one(a2))
  }

  def map[B, V2](f: A => B)(implicit r: Reducer[B, V2]) = two(f(a1), f(a2))

  val measure = v
}
case class Three[V, A](v: V, a1: A, a2: A, a3: A)(implicit r: Reducer[A, V]) extends Finger[V, A] {
  def foldMap[B](f: A => B)(implicit m: Semigroup[B]) = f(a1) |+| f(a2) |+| f(a3)

  def <+:(a: A) = Four(a cons v, a, a1, a2, a3)

  def :+>(a: A) = Four(v snoc a, a1, a2, a3, a)

  def lhead = a1

  def ltail = two(a2, a3)

  def rhead = a3

  def rtail = two(a1, a2)

  def toTree[V](implicit m: Reducer[A, V]) = {
    deep(two(a1, a2), empty[V, Node[V, A]], one(a3))
  }

  def map[B, V2](f: A => B)(implicit r: Reducer[B, V2]) = three(f(a1), f(a2), f(a3))

  val measure = v
}
case class Four[V, A](v: V, a1: A, a2: A, a3: A, a4: A)(implicit r: Reducer[A, V]) extends Finger[V, A] {
  def foldMap[B](f: A => B)(implicit m: Semigroup[B]) = f(a1) |+| f(a2) |+| f(a3) |+| f(a4)

  def <+:(a: A) = error("Digit overflow")

  def :+>(a: A) = error("Digit overflow")

  def lhead = a1

  def ltail = three(a2, a3, a4)

  def rhead = a4

  def rtail = three(a1, a2, a3)

  def toTree[V](implicit m: Reducer[A, V]) = {
    deep(two(a1, a2), empty[V, Node[V, A]], two(a3, a4))
  }

  def map[B, V2](f: A => B)(implicit r: Reducer[B, V2]) = four(f(a1), f(a2), f(a3), f(a4))

  val measure = v
}

sealed abstract class Node[V, A](implicit r: Reducer[A, V]) {
  def fold[B](two: (V, A, A) => B, three: (V, A, A, A) => B): B

  def foldMap[B](f: A => B)(implicit m: Semigroup[B]): B = fold(
    (v, a1, a2) => f(a1) |+| f(a2),
    (v, a1, a2, a3) => f(a1) |+| f(a2) |+| f(a3))

  def toDigit = fold(
    (v, a1, a2) => Two(v, a1, a2),
    (v, a1, a2, a3) => Three(v, a1, a2, a3))

  def measure = this.unit[V]

  def map[B, V2](f: A => B)(implicit m: Reducer[B, V2]) = {
    fold((v, a1, a2) => node2(f(a1), f(a2)),
      (v, a1, a2, a3) => node3(f(a1), f(a2), f(a3)))
  }
}

sealed abstract class FingerTree[V, A](implicit measurer: Reducer[A, V]) {
  def measure = this.unit[V]

  def foldMap[B](f: A => B)(implicit s: Monoid[B]): B =
    fold(s.zero, f, (v, pr, m, sf) => pr.foldMap(f) |+| m.foldMap(x => x.foldMap(f)) |+| sf.foldMap(f))

  def fold[B](empty: => B, single: A => B, deep: (V, Finger[V, A], FingerTree[V, Node[V, A]], Finger[V, A]) => B): B

  def <+:(a: A): FingerTree[V, A] = {
    implicit val nm = NodeMeasure[A, V]
    fold(single(a), b => deep(one(a), empty[V, Node[V, A]], one(b)), (v, pr, m, sf) =>
      pr match {
        case Four(v, b, c, d, e) => deep(two(a, b), node3(c, d, e) <+: m, sf)
        case _ => deep(a <+: pr, m, sf)
      })
  }

  def :+>(a: A): FingerTree[V, A] = {
    implicit val nm = NodeMeasure[A, V]
    fold(single(a), b => deep(one(b), empty[V, Node[V, A]], one(a)), (v, pr, m, sf) =>
      sf match {
        case Four(v, b, c, d, e) => deep(pr, (m :+> node3(b, c, d)), two(e, a))
        case _ => deep(pr, m, sf :+> a)
      })
  }

  def isEmpty = fold(true, x => false, (v, pr, m, sf) => false)

  def viewl: ViewL[PartialApply1Of2[FingerTree, V]#Apply, A] =
    fold(EmptyL[PartialApply1Of2[FingerTree, V]#Apply, A],
      x => OnL[PartialApply1Of2[FingerTree, V]#Apply, A](x, empty), (v, pr, m, sf) =>
        pr match {
          case One(v, x) => OnL[PartialApply1Of2[FingerTree, V]#Apply, A](x, m.viewl.fold(sf.toTree, (a, mm) => deep(a.toDigit, mm, sf)))
          case _ => OnL[PartialApply1Of2[FingerTree, V]#Apply, A](pr.lhead, deep(pr.ltail, m, sf))
        })

  def viewr: ViewR[PartialApply1Of2[FingerTree, V]#Apply, A] =
    fold(EmptyR[PartialApply1Of2[FingerTree, V]#Apply, A],
      x => OnR[PartialApply1Of2[FingerTree, V]#Apply, A](empty, x), (v, pr, m, sf) =>
        pr match {
          case One(v, x) => OnR[PartialApply1Of2[FingerTree, V]#Apply, A](m.viewr.fold(pr.toTree, (mm, a) => deep(pr, mm, a.toDigit)), x)
          case _ => OnR[PartialApply1Of2[FingerTree, V]#Apply, A](deep(pr, m, sf.rtail), sf.rhead)
        })

  def map[B, V2](f: A => B)(implicit m: Reducer[B, V2]): FingerTree[V2, B] = {
    implicit val nm = NodeMeasure[B, V2]
    fold(empty,
      x => single(f(x)),
      (v, pr, mt, sf) => deep(pr map f, mt.map(x => x.map(f)), sf map f))
  }
}

object FingerTree {
  def Node2[V, A](v: V, a1: A, a2: A)(implicit r: Reducer[A, V]) = new Node[V, A] {
    def fold[B](two: (V, A, A) => B, three: (V, A, A, A) => B) =
      two(v, a1, a2)
  }

  def Node3[V, A](v: V, a1: A, a2: A, a3: A)(implicit r: Reducer[A, V]) = new Node[V, A] {
    def fold[B](two: (V, A, A) => B, three: (V, A, A, A) => B) =
      three(v, a1, a2, a3)
  }

  def EmptyR[S[_], A] = new ViewR[S, A] {
    def fold[B](b: => B, f: (S[A], A) => B) = b
  }

  def OnR[S[_], A](sa: S[A], a: A) = new ViewR[S, A] {
    def fold[B](b: => B, f: (S[A], A) => B) = f(sa, a)
  }

  def EmptyL[S[_], A] = new ViewL[S, A] {
    def fold[B](b: => B, f: (A, S[A]) => B) = b
  }

  def OnL[S[_], A](a: A, sa: S[A]) = new ViewL[S, A] {
    def fold[B](b: => B, f: (A, S[A]) => B) = f(a, sa)
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
    Reducer((a: FingerTree[V, A]) => a.fold(m.monoid.zero, x => x.unit[V], (v, x, y, z) => v))
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

  def mappendVal[V, A](v: V, t: FingerTree[V, A])(implicit measure: Reducer[A, V]) = {
    lazy val lv = v snoc t
    t.fold(v, x => lv, (x, p, m, s) => lv)
  }

  def empty[V, A](implicit ms: Reducer[A, V]) = new FingerTree[V, A] {
    def fold[B](b: => B, s: A => B, d: (V, Finger[V, A], FingerTree[V, Node[V, A]], Finger[V, A]) => B): B = b
  }

  def single[V, A](a: A)(implicit ms: Reducer[A, V]) = new FingerTree[V, A] {
    def fold[B](b: => B, s: A => B, d: (V, Finger[V, A], FingerTree[V, Node[V, A]], Finger[V, A]) => B): B = s(a)
  }

  def deep[V, A](pr: Finger[V, A], m: FingerTree[V, Node[V, A]], sf: Finger[V, A])
             (implicit ms: Reducer[A, V]) =
    new FingerTree[V, A] {
      implicit val nodeMeasure = NodeMeasure[A, V]

      def fold[B](b: => B, f: A => B, d: (V, Finger[V, A], FingerTree[V, Node[V, A]], Finger[V, A]) => B): B =
        d(mappendVal(pr.unit[V], m) snoc sf, pr, m, sf)
    }

  /*implicit def FingerShow[V: Show, A: Show]: Show[Finger[V, A]]

  implicit def FingerTreeShow[V: Show, A: Show]: Show[FingerTree[V, A]] = show((t: FingerTree[V, A]) =>
    t.fold("Empty".toList, x => "Single(".toList ++ x.show ++ ")",
      (v, pf, it, sf) => "Deep(".toList ++ v.show ++ "," ++ pf.show ++ "," ++ it.show ++ "," ++ sf.show ++ ")")
  )*/
}
