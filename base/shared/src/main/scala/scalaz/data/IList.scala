package scalaz
package data

import typeclass.IsCovariantClass

trait IListModule {
  type IList[A]

  def uncons[A](as: IList[A]): Maybe2[A, IList[A]]

  implicit def isCovariantInstance: IsCovariant[IList]
}

private[data] object IListImpl extends IListModule {
  type IList[A] = Fix[Maybe2[A, ?]]

  def uncons[A](as: IList[A]): Maybe2[A, IList[A]] =
    Fix.unfix[Maybe2[A, ?]](as)

  implicit val isCovariantInstance: IsCovariant[IList] =
    new IsCovariantClass[IList] with IsCovariantClass.LiftLiskov[IList] {

      override def liftLiskov[A, B](implicit ev: A <~< B): IList[A] <~< IList[B] = {
        type <~~<[F[_], G[_]] = ∀.Prototype[λ[α => F[α] <~< G[α]]]
        val ev1 = Λ[α](Maybe2.isCovariant_1[α].liftLiskov[A, B]): Maybe2[A, ?] <~~< Maybe2[B, ?]
        Fix.liftLiskov[Maybe2[A, ?], Maybe2[B, ?]](ev1.make)(Maybe2.isCovariant_2[A])
      }
    }
}
