package scalaz
package typelevel

trait HFold[M[_], U] {

  type Init <: U
  def init: Init

  type Apply[E, A <: U] <: U
  def apply[E, A <: U](elem: M[E], acc: A): Apply[E, A]

}

object HFold {

  final class Count[M[_]] extends HFold[M, Int] {

    type Init = Int
    def init = 0

    type Apply[E, A <: Int] = Int
    def apply[E, A <: Int](elem: M[E], acc: A) = acc + 1

  }

  final class Reverse[M[_]] extends HFold[M, GenericList[M]] {

    private type U = GenericList[M]

    type Init = GenericNil[M]
    def init = GenericNil[M]()

    type Apply[E, A <: U] = A#Appended[M, GenericCons[M, E, GenericNil[M]]]
    def apply[E, A <: U](elem: M[E], acc: A): Apply[E, A] = acc.append[M, GenericCons[M, E, GenericNil[M]]](GenericCons[M, E, GenericNil[M]](elem, init))

  }

  final class Append[M[_], L <: GenericList[M]](list: L) extends HFold[M, GenericList[M]] {

    private type U = GenericList[M]

    type Init = L
    def init = list

    type Apply[E, A <: U] = GenericCons[M, E, A]
    def apply[E, A <: U](elem: M[E], acc: A): Apply[E, A] = GenericCons[M, E, A](elem, acc)

  }

  final class PrependToHStream[M[_], T[_ <: Nat]](list: HStream[T]) extends HFold[M, AbstractHStream] {

    type Init = HStream[T]
    def init = list

    type Apply[E, A <: AbstractHStream] = A#Prepended[M[E]]
    def apply[E, A <: AbstractHStream](elem: M[E], acc: A): Apply[E, A] = elem :: acc

  }

}

// vim: expandtab:ts=2:sw=2
