package scalaz
package data

import core.EqClass
import debug.DebugClass
import types.IsCovariantClass

import scala.annotation.tailrec
import scala.PartialFunction.{ cond, condOpt }
import scala.inline

trait IListModule {
  type IList[A]

  def empty[A]: IList[A]
  def cons[A](a: A, as: IList[A]): IList[A]
  def uncons[A](as: IList[A]): Maybe2[A, IList[A]]

  object Cons {
    def apply[A](a: A, as: IList[A]): IList[A] = cons(a, as)
    def unapply[A](as: IList[A]): scala.Option[(A, IList[A])] =
      condOpt(uncons(as)) {
        case Maybe2.Just2(h, t) => (h, t)
      }
  }

  object Empty {
    def apply[A](): IList[A] = empty[A]
    def unapply[A](as: IList[A]): Boolean =
      cond(uncons(as)) {
        case Maybe2.Empty2() => true
      }
  }

  def apply[A](as: A*): IList[A] =
    as.foldRight(empty[A])(cons)

  implicit def isCovariantInstance: IsCovariant[IList]
}

trait IListSyntax {
  implicit final class ToIListOps[A](self: IList[A]) {

    def head: Maybe[A] =
      IList.uncons(self) match {
        case Maybe2.Empty2()    => Maybe.empty
        case Maybe2.Just2(a, _) => Maybe.just(a)
      }

    def tail: Maybe[IList[A]] =
      IList.uncons(self) match {
        case Maybe2.Empty2()     => Maybe.empty
        case Maybe2.Just2(_, as) => Maybe.just(as)
      }

    def ::(a: A): IList[A] = IList.cons(a, self)

    def uncons: Maybe2[A, IList[A]] =
      IList.uncons(self)

    def foldLeft[B](z: B)(f: (B, A) => B): B = {
      @tailrec
      def go(acc: B, as: IList[A]): B =
        IList.uncons(as) match {
          case Maybe2.Empty2() => acc
          case Maybe2.Just2(a, aas) =>
            go(f(acc, a), aas)
        }
      go(z, self)
    }

    def foldRight[B](z: B)(f: (A, B) => B): B =
      self.foldLeft(z)((b, a) => f(a, b))

    def append(that: IList[A]): IList[A] =
      that.reverse.foldLeft(self)((b, a) => IList.cons(a, b))

    def :::(that: IList[A]): IList[A] =
      self.append(that)

    def ++(that: IList[A]): IList[A] =
      self.append(that)

    def reverse: IList[A] =
      self.foldLeft(IList.empty[A])((b, a) => a :: b)

    def reverse_:::[Z](that: IList[A]): IList[A] =
      that.foldLeft(self)((as, a) => a :: as)

    def filter(p: A => Boolean): IList[A] =
      self.foldLeft(IList.empty[A])((b, a) => if (p(a)) a :: b else b)

    def zip[B](that: IList[B]): IList[(A, B)] = {
      @tailrec
      def go(acc: IList[(A, B)], as: IList[A], bs: IList[B]): IList[(A, B)] =
        (IList.uncons(as), IList.uncons(bs)) match {
          case (Maybe2.Just2(a, aas), Maybe2.Just2(b, bbs)) =>
            go((a, b) :: acc, aas, bbs)
          case _ => acc.reverse
        }

      go(IList.empty, self, that)
    }

    def take(n: Int): IList[A] = {
      @tailrec
      def go(m: Int, acc: IList[A], as: IList[A]): IList[A] =
        if (m == 0) acc
        else
          IList.uncons(as) match {
            case Maybe2.Empty2() => acc
            case Maybe2.Just2(a, aas) =>
              go(m - 1, a :: acc, aas)
          }

      if (n <= 0) IList.empty
      else
        go(n, IList.empty, self).reverse
    }

    def drop(n: Int): IList[A] = {
      @tailrec
      def go(m: Int, as: IList[A]): IList[A] =
        if (m == 0) as
        else
          IList.uncons(as) match {
            case Maybe2.Empty2() => as
            case Maybe2.Just2(_, aas) =>
              go(m - 1, aas)
          }

      if (n <= 0) self
      else go(n, self)
    }

    def size: Int =
      self.foldLeft(0)((b, _) => 1 + b)
  }

}

trait IListInstances {
  implicit final def listEq[A](implicit A: Eq[A]): Eq[IList[A]] =
    instanceOf[EqClass[IList[A]]] { (a1, a2) =>
      @tailrec def go(l1: IList[A], l2: IList[A]): Boolean =
        (IList.uncons(l1), IList.uncons(l2)) match {
          case (Maybe2.Empty2(), Maybe2.Empty2()) => true
          case (Maybe2.Just2(x, xs), Maybe2.Just2(y, ys)) =>
            if (A.equal(x, y)) go(xs, ys)
            else false
          case _ => false
        }

      go(a1, a2)
    }

  implicit final def ilistDebug[A](implicit A: Debug[A]): Debug[IList[A]] =
    DebugClass.instance(a => {
      import Scalaz.debugInterpolator
      def commaSep(tail: IList[A], acc: Cord): Cord = tail match {
        case IList.Cons(x, xs) =>
          commaSep(xs, Cord.concat(acc, Cord.cons(",", Debug[A].debug(x))))
        case _ =>
          acc
      }

      a match {
        case IList.Cons(h, t) => z"IList(${commaSep(t, A.debug(h))})"
        case _                => Cord("INil")
      }
    })
}

private[data] object IListImpl extends IListModule {
  type IList[A] = Fix[Maybe2[A, ?]]

  def empty[A]: IList[A] =
    Fix.fix[Maybe2[A, ?]](Maybe2.empty2)

  def cons[A](a: A, as: Fix[Maybe2[A, ?]]): IList[A] =
    Fix.fix[Maybe2[A, ?]](Maybe2.just2(a, as))

  def uncons[A](as: IList[A]): Maybe2[A, IList[A]] =
    Fix.unfix[Maybe2[A, ?]](as)

  implicit val isCovariantInstance: IsCovariant[IList] = instanceOf(new IsCovariantClass.LiftLiskov[IList] {

    override def liftLiskov[A, B](implicit ev: A <~< B): IList[A] <~< IList[B] = {
      type <~~<[F[_], G[_]] = ∀.Prototype[λ[α => F[α] <~< G[α]]]
      val ev1 = Λ[α](Maybe2.isCovariant_1[α].liftLiskov[A, B]): Maybe2[A, ?] <~~< Maybe2[B, ?]
      Fix.liftLiskov[Maybe2[A, ?], Maybe2[B, ?]](ev1.make)(Maybe2.isCovariant_2[A])
    }
  })
}

trait IListFunctions {
  @inline def cons[A](a: A, as: IList[A]): IList[A] =
    IList.cons(a, as)

  @inline def uncons[A](as: IList[A]): Maybe2[A, IList[A]] =
    IList.uncons(as)
}
