package scalaz
package typelevel

trait HListsLow {

  import GenericLists._

  // Kleisli proofs for `M` == `Id`

  implicit def baseComposeProof[D <: Direction, H, R] = 
    baseKleisliProof[D, Id, H, R]

  implicit def consComposeRevProof[OH, IH, R, T <: HList](implicit proof: KleisliProof[Reverse, Id, IH, R, T]) =
    consKleisliRevProof[Id, OH, IH, R, T](proof)

  implicit def consComposeProof[H, OR, IR, T <: HList](implicit proof: KleisliProof[Forward, Id, H, IR, T]) =
    consKleisliProof[Id, H, OR, IR, T](proof)

}

trait HLists extends HListsLow {

  import GenericLists._

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

  def HNil: HNil = GenericNil[Id]

}

object HList extends HLists

// vim: expandtab:ts=2:sw=2

