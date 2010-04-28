package scalaz

import scalaz.Scalaz._

sealed abstract class ViewL[S[_], A] {
  def fold[B](b: => B, f: (A, S[A]) => B): B
}

sealed abstract class ViewR[S[_], A] {
  def fold[B](b: => B, f: (S[A], A) => B): B
}

import FingerTree._

sealed abstract class Finger[A] {
  def foldMap[B](f: A => B)(implicit m: Semigroup[B]): B

  def <+:(a: A): Finger[A]

  def :+>(a: A): Finger[A]

  def lhead: A

  def ltail: Finger[A]

  def rhead: A

  def rtail: Finger[A]

  def toTree[V](implicit m: Measured[V, A]): FingerTree[V, A]

  def map[B](f: A => B): Finger[B]

  def measure[V](implicit m: Measured[V, A]) = FingerMeasured(m)(this)
}
case class One[A](a1: A) extends Finger[A] {
  def foldMap[B](f: A => B)(implicit m: Semigroup[B]) = f(a1)

  def <+:(a: A) = Two(a1, a)

  def :+>(a: A) = Two(a, a1)

  def lhead = a1

  def ltail = error("Tail on the digit One")

  def rhead = a1

  def rtail = error("Tail on the digit One")

  def toTree[V](implicit m: Measured[V, A]) = FingerTrees()(m.vm).single(a1)

  def map[B](f: A => B) = One(f(a1))
}
case class Two[A](a1: A, a2: A) extends Finger[A] {
  def foldMap[B](f: A => B)(implicit m: Semigroup[B]) = f(a1) |+| f(a2)

  def <+:(a: A) = Three(a1, a2, a)

  def :+>(a: A) = Three(a, a1, a2)

  def lhead = a1

  def ltail = One(a2)

  def rhead = a2

  def rtail = One(a1)

  def toTree[V](implicit m: Measured[V, A]) = {
    val module = FingerTrees()(m.vm)
    import module._
    implicit val nm = NodeMeasured(m)
    deep(One(a1), empty[Node[V, A]], One(a2))
  }

  def map[B](f: A => B) = Two(f(a1), f(a2))
}
case class Three[A](a1: A, a2: A, a3: A) extends Finger[A] {
  def foldMap[B](f: A => B)(implicit m: Semigroup[B]) = f(a1) |+| f(a2) |+| f(a3)

  def <+:(a: A) = Four(a1, a2, a3, a)

  def :+>(a: A) = Four(a, a1, a2, a3)

  def lhead = a1

  def ltail = Two(a2, a3)

  def rhead = a3

  def rtail = Two(a1, a2)

  def toTree[V](implicit m: Measured[V, A]) = {
    val module = FingerTrees()(m.vm)
    import module._
    implicit val nm = NodeMeasured(m)
    deep(Two(a1, a2), empty[Node[V, A]], One(a3))
  }

  def map[B](f: A => B) = Three(f(a1), f(a2), f(a3))
}
case class Four[A](a1: A, a2: A, a3: A, a4: A) extends Finger[A] {
  def foldMap[B](f: A => B)(implicit m: Semigroup[B]) = f(a1) |+| f(a2) |+| f(a3) |+| f(a4)

  def <+:(a: A) = error("Digit overflow")

  def :+>(a: A) = error("Digit overflow")

  def lhead = a1

  def ltail = Three(a2, a3, a4)

  def rhead = a4

  def rtail = Three(a1, a2, a3)

  def toTree[V](implicit m: Measured[V, A]) = {
    val module = FingerTrees()(m.vm)
    import module._
    implicit val nm = NodeMeasured(m)
    deep(Two(a1, a2), empty[Node[V, A]], Two(a3, a4))
  }

  def map[B](f: A => B) = Four(f(a1), f(a2), f(a3), f(a4))
}

sealed abstract class Node[V, A] {
  def fold[B](two: (V, A, A) => B, three: (V, A, A, A) => B): B

  def foldMap[B](f: A => B)(implicit m: Semigroup[B]): B = fold(
    (v, a1, a2) => f(a1) |+| f(a2),
    (v, a1, a2, a3) => f(a1) |+| f(a2) |+| f(a3))

  def toDigit = fold(
    (v, a1, a2) => Two(a1, a2),
    (v, a1, a2, a3) => Three(a1, a2, a3))

  def measure(implicit m: Measured[V, A]) = NodeMeasured(m)(this)

  def map[V2, B](f: A => B)(implicit m: Measured[V2, B]) = {
    val mk = module(m.vm)
    fold((v, a1, a2) => mk.node2(f(a1), f(a2)),
      (v, a1, a2, a3) => mk.node3(f(a1), f(a2), f(a3)))
  }
}

sealed abstract class FingerTree[V, A] {
  implicit val measure: Measured[V, A]
  implicit val module: FingerTrees[V]
  import module._

  def measure(implicit m: Measured[V, A]) = FingerTreeMeasured(m)(this)

  def foldMap[B](f: A => B)(implicit s: Monoid[B]): B =
    fold(s.zero, f, (v, pr, m, sf) => pr.foldMap(f) |+| m.foldMap(x => x.foldMap(f)) |+| sf.foldMap(f))

  def fold[B](empty: => B, single: A => B, deep: (V, Finger[A], FingerTree[V, Node[V, A]], Finger[A]) => B): B

  def <+:(a: A): FingerTree[V, A] = {
    implicit val nm = NodeMeasured(measure)
    fold(single(a), b => deep(One(a), empty[Node[V, A]], One(b)), (v, pr, m, sf) =>
      pr match {
        case Four(b, c, d, e) => deep(Two(a, b), node3(c, d, e) <+: m, sf)
        case _ => deep(a <+: pr, m, sf)
      })
  }

  def :+>(a: A): FingerTree[V, A] = {
    implicit val nm = NodeMeasured(measure)
    fold(single(a), b => deep(One(b), empty[Node[V, A]], One(a)), (v, pr, m, sf) =>
      sf match {
        case Four(b, c, d, e) => deep(pr, (m :+> node3(b, c, d)), Two(e, a))
        case _ => deep(pr, m, sf :+> a)
      })
  }

  def isEmpty = fold(true, x => false, (v, pr, m, sf) => false)

  def viewl: ViewL[PartialApply1Of2[FingerTree, V]#Apply, A] =
    fold(EmptyL[PartialApply1Of2[FingerTree, V]#Apply, A],
      x => OnL[PartialApply1Of2[FingerTree, V]#Apply, A](x, empty), (v, pr, m, sf) =>
        pr match {
          case One(x) => OnL[PartialApply1Of2[FingerTree, V]#Apply, A](x, m.viewl.fold(sf.toTree, (a, mm) => deep(a.toDigit, mm, sf)))
          case _ => OnL[PartialApply1Of2[FingerTree, V]#Apply, A](pr.lhead, deep(pr.ltail, m, sf))
        })

  def viewr: ViewR[PartialApply1Of2[FingerTree, V]#Apply, A] =
    fold(EmptyR[PartialApply1Of2[FingerTree, V]#Apply, A],
      x => OnR[PartialApply1Of2[FingerTree, V]#Apply, A](empty, x), (v, pr, m, sf) =>
        pr match {
          case One(x) => OnR[PartialApply1Of2[FingerTree, V]#Apply, A](m.viewr.fold(pr.toTree, (mm, a) => deep(pr, mm, a.toDigit)), x)
          case _ => OnR[PartialApply1Of2[FingerTree, V]#Apply, A](deep(pr, m, sf.rtail), sf.rhead)
        })

  def map[V2, B](f: A => B)(implicit m: Measured[V2, B]): FingerTree[V2, B] = {
    val mk = FingerTree.module(m.vm)
    implicit val nm = NodeMeasured[V2, B](m)
    fold(mk.empty,
      x => mk.single(f(x)),
      (v, pr, mt, sf) => mk.deep(pr map f, mt.map(x => x.map(f)), sf map f))
  }
}

abstract class Measured[V, A](m: Monoid[V]) {
  val vm = m

  def apply(a: A): V
}

object FingerTree {
  def Node2[V, A](v: V, a1: A, a2: A) = new Node[V, A] {
    def fold[B](two: (V, A, A) => B, three: (V, A, A, A) => B) =
      two(v, a1, a2)
  }

  def Node3[V, A](v: V, a1: A, a2: A, a3: A) = new Node[V, A] {
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

  def module[V](implicit m: Monoid[V]) = {
    FingerTrees[V]
  }

  def measured[V, A](f: A => V)(implicit m: Monoid[V]) {
    new Measured[V, A](m) {
      def apply(a: A) = f(a)
    }
  }

  implicit def FingerMeasured[V, A](m: Measured[V, A]) = new Measured[V, Finger[A]](m.vm) {
    def apply(a: Finger[A]): V = a.foldMap[V](m.apply(_))(m.vm)
  }

  implicit def NodeMeasured[V, A](m: Measured[V, A]) = new Measured[V, Node[V, A]](m.vm) {
    def apply(a: Node[V, A]): V = a fold (
            (v, _, _) => v,
            (v, _, _, _) => v)
  }

  implicit def FingerTreeMeasured[V, A](m: Measured[V, A]): Measured[V, FingerTree[V, A]] = new Measured[V, FingerTree[V, A]](m.vm) {
    def apply(a: FingerTree[V, A]): V = a.fold(m.vm.zero, x => m(x), (v, x, y, z) => v)
  }
}

case class FingerTrees[V](implicit vm: Monoid[V]) {
  def node2[A](a: A, b: A)(implicit measure: Measured[V, A]) =
    Node2[V, A](measure(a) |+| measure(b), a, b)

  def node3[A](a: A, b: A, c: A)(implicit measure: Measured[V, A]) =
    Node3[V, A](measure(a) |+| (measure(b) |+| measure(c)), a, b, c)

  def mappendVal[A](v: V, t: FingerTree[V, A])(implicit measure: Measured[V, A]) = {
    lazy val lv = v |+| FingerTreeMeasured(measure)(t)
    t.fold(v, x => lv, (x, p, m, s) => lv)
  }

  def empty[A](implicit ms: Measured[V, A]) = new FingerTree[V, A] {
    implicit val module = FingerTrees.this
    implicit val measure = ms

    def fold[B](b: => B, s: A => B, d: (V, Finger[A], FingerTree[V, Node[V, A]], Finger[A]) => B): B = b
  }

  def single[A](a: A)(implicit ms: Measured[V, A]) = new FingerTree[V, A] {
    implicit val module = FingerTrees.this
    implicit val measure = ms

    def fold[B](b: => B, s: A => B, d: (V, Finger[A], FingerTree[V, Node[V, A]], Finger[A]) => B): B = s(a)
  }

  def deep[A](pr: Finger[A], m: FingerTree[V, Node[V, A]], sf: Finger[A])
             (implicit ms: Measured[V, A]) =
    new FingerTree[V, A] {
      implicit val module = FingerTrees.this
      implicit val measure = ms
      implicit val nodeMeasure = NodeMeasured(measure)

      def fold[B](b: => B, f: A => B, d: (V, Finger[A], FingerTree[V, Node[V, A]], Finger[A]) => B): B =
        d(mappendVal(FingerMeasured(measure)(pr), m) |+| FingerMeasured(measure)(sf), pr, m, sf)
    }
}
