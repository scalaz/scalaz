package scalaz
package typelevel

trait HListsLow { self: HLists =>

  // Kleisli proofs for `M` == `Id`

  implicit def baseComposeProof[D <: Direction, H, R] = 
    baseKleisliProof[D, Id, H, R]

  implicit def consComposeRevProof[OH, IH, R, T <: HList](implicit proof: KleisliProof[Reverse, Id, IH, R, T]) =
    consKleisliRevProof[Id, OH, IH, R, T](proof)

  implicit def consComposeProof[H, OR, IR, T <: HList](implicit proof: KleisliProof[Forward, Id, H, IR, T]) =
    consKleisliProof[Id, H, OR, IR, T](proof)

}

trait HLists extends HListsLow {

  final class IdOps[T <: HList](list: T) {

    def ::[A](elem: A): HCons[A, T] = GenericCons[Id, A, T](elem, list)

    def compose[M[_], H, R](implicit ev: KleisliProof[Forward, M, H, R, T], b: Bind[M]): Kleisli[M, H, R] =
      ev(list)

    def reverseCompose[M[_], H, R](implicit ev: KleisliProof[Reverse, M, H, R, T], b: Bind[M]): Kleisli[M, H, R] =
      ev(list)

  }

  implicit def mkIdOps[T <: HList](list: T): IdOps[T] = new IdOps(list)

  type HList = GenericList[Id]
  type HCons[H, T <: HList] = GenericCons[Id, H, T]
  type HNil = GenericNil[Id]

  def HNil: HNil = GenericNil[Id]()

  object HCons {

    def apply[H, T <: HList](head: H, tail: T): HCons[H, T] = GenericCons[Id, H, T](head, tail)

    def unapply[H, T <: HList](list: HCons[H, T]): Option[(H, T)] = Some(list.head, list.tail)

  }

  type ::[H, T <: HList] = HCons[H, T]
  val :: = HCons

  // Kleisli proofs

  import Kleisli._

  sealed trait Direction
  final class Forward extends Direction
  final class Reverse extends Direction

  sealed trait KleisliProof[D <: Direction, M[_], H, R, T <: HList] {
    def apply(list: T)(implicit b: Bind[M]): Kleisli[M, H, R]
  }

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

// vim: expandtab:ts=2:sw=2

