package scalaz

import scalaz.Scalaz._

sealed abstract class ViewL[+S[_], +A]
case object EmptyL extends ViewL[Nothing, Nothing]
case class &:[S[_], A](a: A, sa: S[A]) extends ViewL[S, A]

sealed abstract class ViewR[+S[_], +A]
case object EmptyR extends ViewR[Nothing, Nothing]
final case class :&[S[_], A](sa: S[A], a: A) extends ViewR[S, A]

sealed abstract class Finger[A] {
  def foldMap[B](f: A => B)(implicit m: Semigroup[B]): B
}
case class One[A](a1: A) extends Finger[A] {
  def foldMap[B](f: A => B)(implicit m: Semigroup[B]) = f(a1)
}
case class Two[A](a1: A, a2: A) extends Finger[A] {
  def foldMap[B](f: A => B)(implicit m: Semigroup[B]) = m.append(f(a1), f(a2))
}
case class Three[A](a1: A, a2: A, a3: A) extends Finger[A] {
  def foldMap[B](f: A => B)(implicit m: Semigroup[B]) = m.append(f(a1), m.append(f(a2), f(a3)))
}
case class Four[A](a1: A, a2: A, a3: A, a4: A) extends Finger[A] {
  def foldMap[B](f: A => B)(implicit m: Semigroup[B]) = m.append(f(a1), m.append(f(a2), m.append(f(a3), f(a4))))
}

trait Measured[A] {
  type V
  val vm: Monoid[V]
  def apply(a: A): V
}

case class FingerMeasured[A, M <: Measured[A], S <: Monoid[M#V]](implicit m: M, s: S) extends Measured[Finger[A]] {
  type V = M#V
  val vm = s
  def apply(a: Finger[A]):V = a.foldMap[V](m.apply(_))(vm)
}

sealed abstract class Node[V, A] {
  def foldMap[B](f: A => B)(implicit m: Semigroup[B])
}
case class Node2[V, A](v: V, a1: A, a2: A) extends Node[V, A] {
  def foldMap[B](f: A => B)(implicit m: Semigroup[B]) = m.append(f(a1), f(a2))
}
case class Node3[V, A](v: V, a1: A, a2: A, a3: A) extends Node[V, A] {
  def foldMap[B](f: A => B)(implicit m: Semigroup[B]) = m.append(f(a1), m.append(f(a2), f(a3)))
}

case class NodeMeasured[A, M <: Measured[A], S <: Monoid[M#V]](implicit m: M, s: S) extends Measured[Node[M#V, A]] {
  type V = M#V
  val vm = s
  def apply(a: Node[V, A]) = a match {
    case Node2(v, _, _) => v
    case Node3(v, _, _, _) => v
  }
}

sealed abstract class FingerTree[V, A] {
  def foldMap[B](f: A => B)(implicit s: Monoid[B])
  def mapTree[B, M <: Measured[B]](implicit s: M)
}
case class EmptyFT[V, A]() extends FingerTree[V, A] {
  def foldMap[B](f: Any => B)(implicit s: Monoid[B]) = s.zero
}
case class SingleFT[V, A](a: A) extends FingerTree[V, A] {
  def foldMap[B](f: A => B)(implicit s: Monoid[B]) = f(a)
}
case class DeepFT[V, A](v: V, pr: Finger[A], m: FingerTree[V, Node[V, A]], sf: Finger[A]) {
  def foldMap[B](f: A => B)(implicit s: Monoid[B]) = s.append(pr.foldMap(f), s.append(m.foldMap(_.foldMap(f)), sf.foldMap(f)))
}
  
case class FingerTreeMeasured[A, M <: Measured[A]](implicit m: M) extends Measured[FingerTree[M#V, A]] {
  type V = M#V
  val vm = m.vm
  def apply(a: FingerTree[V, A]) = a match {
    case EmptyFT() => vm.zero
    case SingleFT(x) => m(x)
    case DeepFT(v: V, _: FingerTree[V, A], _, _) => v: V
  }
}

object FingerTree {
  def node2[A, M <: Measured[A]](a: A, b: A)(implicit m: M) =
    Node2[M#V, A](m.vm.append(m(a), m(b)), a, b)
  def node3[A, M <: Measured[A]](a: A, b: A, c: A)(implicit m: M) =
    Node3[M#V, A](m.vm.appenf(m(a), m.vm.append(m(b), m(c))), a, b, c)
  def nodeToFinger[A, M <: Measured[A]](n: Node[M#V, A]) = n match {
    case Node2(_, a, b) => Two(a, b)
    case Node3(_, a, b, c) => Three(a, b, c)
  }
  def mappendVal[A, M <: Measured[A]](v: M#V, t: FingerTree[M#V, A])(implicit s: M) = t match {
    case empty => v
    case _ => s.vm.append(v, s(t))
  }
  def deep[A, M <: Measured[A]](pr: Finger[A], m: FingerTree[M#V, Node[M#V, A]], sf: Finger[A])(implicit s: M) =
    DeepFT(s.vm.append(mappendVal(s(pr), m), s(sf)), pr, m, sf)
  def empty[A, M <: Measured[A]] = EmptyFT[M#V]
  def single[A, M <: Measured[A]](a: A) = SingleFT[M#V, A](a)
}
