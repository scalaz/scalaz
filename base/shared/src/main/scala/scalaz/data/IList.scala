package scalaz
package data

import typeclass.IsCovariantClass
import scala.annotation.tailrec

trait IListModule {
  type IList[A]

  def uncons[A](as: IList[A]): Maybe2[A, IList[A]]

  def cons[A](a: A, list: IList[A]): IList[A]

  def concat[A](as1: IList[A], as2: IList[A]): IList[A]

  def empty[A]: IList[A]

  implicit val foldable: Foldable[IList]

  implicit def isCovariantInstance: IsCovariant[IList]
}

private[data] object IListImpl extends IListModule {
  type IList[A] = Fix[Maybe2[A, ?]]

  def uncons[A](as: IList[A]): Maybe2[A, IList[A]] =
    Fix.unfix[Maybe2[A, ?]](as)

  def cons[A](a: A, list: IList[A]): IList[A] =
    Fix.fix[Maybe2[A, ?]](Maybe2.just2(a, list))

  def concat[A](i1: IList[A], i2: IList[A]): IList[A] = {
    @tailrec
    def go(as1: IList[A], as2: IList[A]): IList[A] = {
      Maybe2.toOption2(uncons(as1)) match {
        case Some2(a, as) => go(as, cons(a, as2))
        case None2 => as2
      }
    }
    go(reverse(i1), i2)
  }

  def reverse[A](i1: IList[A]): IList[A] = {
    @tailrec
    def go(in: IList[A], out: IList[A]): IList[A] = {
      Maybe2.toOption2(uncons(in)) match {
        case Some2(a, as) => go(as, cons(a, out))
        case None2 => out
      }
    }
    go(i1, empty)
  }

  def empty[A] = Fix.fix(Maybe2.empty2[A, IList[A]])

  implicit val isCovariantInstance: IsCovariant[IList] =
    new IsCovariantClass[IList] with IsCovariantClass.LiftLiskov[IList] {

      override def liftLiskov[A, B](implicit ev: A <~< B): IList[A] <~< IList[B] = {
        type <~~<[F[_], G[_]] = ∀.Prototype[λ[α => F[α] <~< G[α]]]
        val ev1 = Λ[α](Maybe2.isCovariant_1[α].liftLiskov[A, B]): Maybe2[A, ?] <~~< Maybe2[B, ?]
        Fix.liftLiskov[Maybe2[A, ?], Maybe2[B, ?]](ev1.make)(Maybe2.isCovariant_2[A])
      }
    }

  implicit val foldable: Foldable[IList] = new Foldable[IList] {
    def foldMap[A, B](fa: IList[A])(f: A => B)(implicit M: Monoid[B]): B = 
      foldLeft(fa, M.empty)((b, a) => M.semigroup.append(b, f(a)))

    def foldRight[A, B](fa: IList[A], z: => B)(f: (A, => B) => B): B =
      Maybe2.toOption2(uncons(fa)) match {
        case Some2(a, as) => f(a, foldRight(as, z)(f))
        case None2 => z
      }

    def foldLeft[A, B](fa: IList[A], z: B)(f: (B, A) => B): B =
      Maybe2.toOption2(uncons(fa)) match {
        case Some2(a, as) => foldLeft(as, f(z, a))(f)
        case None2 => z
      }

    override def toList[A](fa: IList[A]): List[A] = 
      foldLeft(fa, List.empty[A])((as, a) => a :: as)
  }
}
