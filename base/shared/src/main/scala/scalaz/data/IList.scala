package scalaz
package data

import Prelude._
import scalaz.typeclass.IsCovariant

trait IListModule {
  type IList[A]

  def uncons[A](as: IList[A]): Maybe2[A, IList[A]]

  implicit def isCovariantInstance: IsCovariant[IList]
}

private[data] object IListImpl extends IListModule {
  type IList[A] = Fix[Maybe2[A, ?]]

  def uncons[A](as: IList[A]): Maybe2[A, IList[A]] =
    Fix.unfix[Maybe2[A, ?]](as)

  implicit val isCovariantInstance: IsCovariant[IList] = IsCovariant.witness1[IList] {
    type <~~<[F[_], G[_]] = ∀.Prototype[λ[α => F[α] <~< G[α]]]
    val ev1 = Λ[α](Maybe2.isCovariant_1[α].apply[Void, Any]): Maybe2[Void, ?] <~~< Maybe2[Any, ?]
    Fix.liftLiskov[Maybe2[Void, ?], Maybe2[Any, ?]](ev1.make)(Maybe2.isCovariant_2[Void])
  }
}
