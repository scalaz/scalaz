package scalaz
package typelevel

import annotation.implicitNotFound
import scalaz.{Apply, Kleisli}

sealed trait GenericList[+M[_]] {

  import Typelevel._

  type Transformed[N[_]] <: GenericList[N]
  type Folded[N[X] >: M[X], U, F <: HFold[N, U]] <: U
  type Appended[N[X] >: M[X], L <: GenericList[N]] <: GenericList[N]
  type Function[R]
  type Down <: GenericList[Id]

  def transform[N[_]](trans: M ~> N): Transformed[N]
  def fold[N[X] >: M[X], U, F <: HFold[N, U]](fold: F): Folded[N, U, F]
  def append[N[X] >: M[X], L <: GenericList[N]](list: L): Appended[N, L]
  def apply[N[X] >: M[X] : Apply, R](f: N[Function[R]]): N[R]
  def down: Down

  final def coerce[N[X] >: M[X]]: Transformed[N] = {
    val t = new (M ~> N) {
      def apply[A](ma: M[A]): N[A] = ma
    }
    transform(t)
  }
 
  final def :^:[A, N[X] >: M[X]](elem: N[A]): GenericCons[N, A, this.type] =
    GenericCons[N, A, this.type](elem, this)

  final def at[N[X] >: M[X], I <: Nat, E](index: I)(implicit ev: IndexProof[N, E, I, this.type]): N[E] = ev(this)

}

case class GenericCons[M[_], H, +T <: GenericList[M]](
  head: M[H],
  tail: T
) extends GenericList[M] {

  override type Transformed[N[_]] = GenericCons[N, H, tail.Transformed[N]]
  override type Folded[N[X] >: M[X], U, F <: HFold[N, U]] = F#Apply[H, tail.Folded[N, U, F]]
  override type Appended[N[X] >: M[X], L <: GenericList[N]] = GenericCons[N, H, tail.Appended[N, L]]
  override type Function[R] = H => tail.Function[R]
  override type Down = GenericCons[Id, M[H], tail.Down]

  def transform[N[_]](trans: M ~> N) = GenericCons(trans(head), tail.transform(trans))
  def fold[N[X] >: M[X], U, F <: HFold[N, U]](f: F): Folded[N, U, F] = f(head, tail.fold[N, U, F](f))
  def append[N[X] >: M[X], L <: GenericList[N]](list: L) = GenericCons[N, H, tail.Appended[N, L]](head, tail.append[N, L](list))
  def apply[N[X] >: M[X] : Apply, R](f: N[Function[R]]): N[R] = tail.apply(Apply[N].ap(head)(f))
  def down: Down = GenericCons[Id, M[H], tail.Down](head, tail.down)

}

case class GenericNil[M[_]]() extends GenericList[M] {

  override type Transformed[N[_]] = GenericNil[N]
  override type Folded[N[X] >: M[X], U, F <: HFold[N, U]] = F#Init
  override type Appended[N[X] >: M[X], L <: GenericList[N]] = L
  override type Function[R] = R
  override type Down = GenericNil[Id]

  def transform[N[_]](trans: M ~> N) = GenericNil()
  def fold[N[X] >: M[X], U, F <: HFold[N, U]](fold: F): Folded[N, U, F] = fold.init
  def append[N[X] >: M[X], L <: GenericList[N]](list: L) = list
  def apply[N[X] >: M[X] : Apply, R](f: N[Function[R]]): N[R] = f
  def down: Down = GenericNil[Id]()

}

trait GenericLists {

  // Index access proofs

  import Typelevel._

  @implicitNotFound(msg = "Could not access element at index ${N} in ${T}")
  sealed trait IndexProof[M[_], E, N <: Nat, -T <: GenericList[M]] {
    def apply(list: T): M[E]
  }

  implicit def zeroIndexProof[M[_], H, T <: GenericList[M]]: IndexProof[M, H, _0, GenericCons[M, H, T]] = 
    new IndexProof[M, H, _0, GenericCons[M, H, T]] {
      def apply(list: GenericCons[M, H, T]) = list.head
    }

  implicit def succIndexProof[M[_], H, E, N <: Nat, T <: GenericList[M]](
    implicit ev: IndexProof[M, E, N, T]
  ): IndexProof[M, E, Succ[N], GenericCons[M, H, T]] = 
    new IndexProof[M, E, Succ[N], GenericCons[M, H, T]] {
      def apply(list: GenericCons[M, H, T]) = ev(list.tail)
    }



}

// vim: expandtab:ts=2:sw=2

