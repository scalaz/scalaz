package scalaz
package typelevel

import scalaz.{Apply, Kleisli}

import scalaz.Id._

object GenericList {

  // index-based access via implicits

  import Nats._

  object IndexProof {

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

  @annotation.implicitNotFound(msg = "Could not access element at index ${N} in ${T}")
  sealed trait IndexProof[M[_], E, N <: Nat, -T <: GenericList[M]] {
    def apply(list: T): M[E]
  }


  // special operations on `M` == `Id`

  import HLists._

  implicit def mkIdOps[T <: HList](list: T): IdOps[T] = new IdOps(list)

}

/**
 * A list which preserves the precise types of all elements. It can be seen as
 * a generalized tuple of arbitrary arity.
 *
 * This structure allows to store values of different types together and
 * supports some basic operations like appending, folding or mapping via a
 * [[scalaz.NaturalTransformation]]. It also serves as a replacement for
 * [[scalaz.syntax.ApplicativeBuilder]].
 *
 * All elements have to share a common type constructor, e.g. [[scala.List]]
 * or [[scala.Option]]. This makes easy use with [[scalaz.Applicative]]
 * possible. If the types of the elements are not in appropriate shape,
 * use [[scalaz.Id]].
 *
 * Because this trait is covariant in `M`, most methods have an additional
 * type parameter `N` which is usually inferred to be equal to `M`. However,
 * it is still possible to prepend a value of type `Option[A]` to a list with
 * the type constructor `Some`.
 *
 * @tparam M The common type constructor of all elements.
 * @see [[scalaz.typelevel.HList]]
 */
sealed trait GenericList[+M[_]] {

  import GenericList._

  type Transformed[N[_]] <: GenericList[N]
  type Folded[N[X] >: M[X], U, F <: HFold[N, U]] <: U
  type Appended[N[X] >: M[X], L <: GenericList[N]] <: GenericList[N]
  type Function[R]
  type Down <: GenericList[Id]

  /**
   * Applies a [[scalaz.NaturalTransformation]] to all elements, preserving the
   * overall structure of the list. The result list has the same element types
   * except that every occurence of the original common type constructor `M` is
   * replaced by `N`.
   */
  def transform[N[_]](trans: M ~> N): Transformed[N]

  /**
   * Folds this list using a [[scalaz.typelevel.HFold]].
   */
  def fold[N[X] >: M[X], U, F <: HFold[N, U]](f: F): Folded[N, U, F]

  /**
   * Appends the given list to this list.
   */
  def append[N[X] >: M[X], L <: GenericList[N]](list: L): Appended[N, L]

  /**
   * Successively applies a (curried) function to the elements of this list
   * using the specified [[scalaz.Apply]] instance.
   */
  def apply[N[X] >: M[X] : Apply, R](f: N[Function[R]]): N[R]

  /**
   * Converts this list to a [[scalaz.typelevel.HList]], i.e. replaces every
   * type `M[A]` by `Id[M[A]]`. The resulting list contains exactly the same
   * elements as the resulting list.
   */
  def down: Down

  /**
   * Dependently typed version of `fold` which improves type inference in some
   * circumstances.
   */
  final def foldU[N[X] >: M[X], U](f: HFold[N, U]): Folded[N, U, f.type] = fold[N, U, f.type](f)

  /**
   * Version of `apply` which takes the bare function and wraps it into `N`.
   */
  final def applyP[N[X] >: M[X] : Applicative, R](f: Function[R]): N[R] =
    apply[N, R](Pointed[N].point(f))

  /**
   * Upcasts the type constructor in this list. This operation is safe.
   */
  final def coerce[N[X] >: M[X]]: Transformed[N] = {
    val t = new (M ~> N) {
      def apply[A](ma: M[A]): N[A] = ma
    }
    transform(t)
  }
 
  /**
   * Prepends a value to this list.
   */
  final def :^:[A, N[X] >: M[X]](elem: N[A]): GenericCons[N, A, this.type] =
    GenericCons[N, A, this.type](elem, this)

  /**
   * Accesses an element at a specific index.
   */
  final def at[N[X] >: M[X], I <: Nat, E](index: I)(implicit ev: IndexProof[N, E, I, this.type]): N[E] = ev(this)

}

/** @see [[scalaz.typelevel.GenericList]] */
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

/** @see [[scalaz.typelevel.GenericList]] */
case class GenericNil[M[_]]() extends GenericList[M] {

  override type Transformed[N[_]] = GenericNil[N]
  override type Folded[N[X] >: M[X], U, F <: HFold[N, U]] = F#Init
  override type Appended[N[X] >: M[X], L <: GenericList[N]] = L
  override type Function[R] = R
  override type Down = GenericNil[Id]

  def transform[N[_]](trans: M ~> N) = GenericNil()
  def fold[N[X] >: M[X], U, F <: HFold[N, U]](f: F): Folded[N, U, F] = f.init
  def append[N[X] >: M[X], L <: GenericList[N]](list: L) = list
  def apply[N[X] >: M[X] : Apply, R](f: N[Function[R]]): N[R] = f
  def down: Down = GenericNil[Id]()

}

// vim: expandtab:ts=2:sw=2
