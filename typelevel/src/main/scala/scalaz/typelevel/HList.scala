package scalaz
package typelevel

import Kleisli._

import Id._

object HLists {

  import KleisliProof._

  final class IdOps[T <: HList](list: T) {

    /** Prepends a value to this list. */
    def ::[A](elem: A): HCons[A, T] = GenericCons[Id, A, T](elem, list)

    /**
     * Composes a list of functions of the shape `A => F[B]` using
     * [[scalaz.Kleisli]]. This operation can be seen as a fold with `compose`.
     */
    def compose[M[_], H, R](implicit ev: KleisliProof[Forward, M, H, R, T], b: Bind[M]): Kleisli[M, H, R] =
      ev(list)

    /**
     * Composes a list of functions of the shape `A => F[B]` using
     * [[scalaz.Kleisli]]. This operation can be seen as a fold with `andThen`.
     */
    def reverseCompose[M[_], H, R](implicit ev: KleisliProof[Reverse, M, H, R, T], b: Bind[M]): Kleisli[M, H, R] =
      ev(list)

  }

  trait KleisliProof0 {

    implicit def baseComposeProof[D <: Direction, H, R] =
      baseKleisliProof[D, Id, H, R]

    implicit def consComposeRevProof[OH, IH, R, T <: HList](implicit proof: KleisliProof[Reverse, Id, IH, R, T]) =
      consKleisliRevProof[Id, OH, IH, R, T](proof)

    implicit def consComposeProof[H, OR, IR, T <: HList](implicit proof: KleisliProof[Forward, Id, H, IR, T]) =
      consKleisliProof[Id, H, OR, IR, T](proof)

  }

  object KleisliProof extends KleisliProof0 {

    sealed trait Direction
    final class Forward extends Direction
    final class Reverse extends Direction

    implicit def baseKleisliProof[D <: Direction, M[_], H, R]: KleisliProof[D, M, H, R, HCons[H => M[R], HNil]] =
      new KleisliProof[D, M, H, R, HCons[H => M[R], HNil]] {
        def apply(list: HCons[H => M[R], HNil])(implicit b: Bind[M]) = kleisli(list.head)
      }

    implicit def consKleisliRevProof[M[_], OH, IH, R, T <: HList](
      implicit proof: KleisliProof[Reverse, M, IH, R, T]
    ): KleisliProof[Reverse, M, OH, R, HCons[OH => M[IH], T]] =
      new KleisliProof[Reverse, M, OH, R, HCons[OH => M[IH], T]] {
        def apply(list: HCons[OH => M[IH], T])(implicit b: Bind[M]) = kleisli(list.head) >=> proof(list.tail)
      }

    implicit def consKleisliProof[M[_], H, OR, IR, T <: HList](
      implicit proof: KleisliProof[Forward, M, H, IR, T]
    ): KleisliProof[Forward, M, H, OR, HCons[IR => M[OR], T]] =
      new KleisliProof[Forward, M, H, OR, HCons[IR => M[OR], T]] {
        def apply(list: HCons[IR => M[OR], T])(implicit b: Bind[M]) = kleisli(list.head) <=< proof(list.tail)
      }

  }

  @annotation.implicitNotFound(msg = "Could not compose HList ${T} in ${D} direction")
  sealed trait KleisliProof[D <: Direction, M[_], H, R, T <: HList] {
    def apply(list: T)(implicit b: Bind[M]): Kleisli[M, H, R]
  }

}


// vim: expandtab:ts=2:sw=2
