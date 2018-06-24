package scalaz
package data

import core.EqClass
import debug.DebugClass
import types.IsCovariantClass

import scala.annotation.tailrec
import scala.PartialFunction.{ cond, condOpt }
import scala.AnyVal

trait IListModule {
  type IList[A]

  def empty[A]: IList[A]
  def cons[A](a: A, as: IList[A]): IList[A]
  def uncons[A](as: IList[A]): Maybe2[A, IList[A]]
  def foldLeft[A, B](f: (B, A) => B, z: B, as: IList[A]): B
  def reverse[A](as: IList[A]): IList[A]
  def append[A](as: IList[A], aas: IList[A]): IList[A]

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

final class IListOps[A](val self: IList[A]) extends AnyVal {

  def headOption: Maybe[A] =
    IList.uncons(self) match {
      case Maybe2.Empty2()    => Maybe.empty
      case Maybe2.Just2(a, _) => Maybe.just(a)
    }

  def tailOption: Maybe[IList[A]] =
    IList.uncons(self) match {
      case Maybe2.Empty2()     => Maybe.empty
      case Maybe2.Just2(_, as) => Maybe.just(as)
    }

  def ::(a: A): IList[A] = IList.cons(a, self)

  def uncons: Maybe2[A, IList[A]] =
    IList.uncons(self)

  def foldLeft[B](z: B)(f: (B, A) => B) =
    IList.foldLeft(f, z, self)

  def append(that: IList[A]): IList[A] =
    IList.append(self, that)

  def :::[Z](that: IList[A]): IList[A] =
    IList.foldLeft[A, IList[A]]((b, a) => IList.cons(a, b), self, IList.reverse(that))

  def reverse: IList[A] =
    IList.reverse(self)

  def reverse_:::[Z](that: IList[A]): IList[A] =
    IList.foldLeft[A, IList[A]]((as, a) => IList.cons(a, as), self, that)

  def filter(p: A => Boolean): IList[A] =
    IList.foldLeft[A, IList[A]]((b, a) => if (p(a)) IList.cons(a, b) else b, IList.empty, self)

  def zip[B](that: IList[B]): IList[(A, B)] = {
    @tailrec
    def go(acc: IList[(A, B)], as: IList[A], bs: IList[B]): IList[(A, B)] =
      (IList.uncons(as), IList.uncons(bs)) match {
        case (Maybe2.Just2(a, aas), Maybe2.Just2(b, bbs)) => go(IList.cons((a, b), acc), aas, bbs)
        case _                                            => acc
      }
    go(IList.empty, self, that)
  }

  def take(n: Int): IList[A] = {
    @tailrec
    def go(m: Int, acc: IList[A], as: IList[A]): IList[A] =
      IList.uncons(as) match {
        case Maybe2.Empty2() => IList.reverse(acc)
        case Maybe2.Just2(a, aas) =>
          go(m - 1, IList.cons(a, acc), aas)
      }
    if (n <= 0) IList.empty
    else go(n, IList.empty, self)
  }

  def drop(n: Int): IList[A] = {
    @tailrec
    def go(m: Int, as: IList[A]): IList[A] =
      IList.uncons(as) match {
        case Maybe2.Empty2() => as
        case Maybe2.Just2(_, aas) =>
          go(m - 1, aas)
      }
    if (n <= 0) self
    else go(n, self)
  }

  def size: Int =
    IList.foldLeft[A, Int]((b, a) => 1 + b, 0, self)
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

  @tailrec
  def foldLeft[A, B](f: (B, A) => B, z: B, as: IList[A]): B =
    uncons(as) match {
      case Maybe2.Empty2() => z
      case Maybe2.Just2(a, aas) =>
        foldLeft(f, f(z, a), aas)
    }

  def append[A](as: IList[A], aas: IList[A]): IList[A] =
    foldLeft[A, IList[A]]((b, a) => cons(a, b), aas, reverse(as))

  def reverse[A](as: IList[A]): IList[A] =
    foldLeft[A, IList[A]]((b, a) => cons(a, b), empty, as)

  implicit val isCovariantInstance: IsCovariant[IList] = instanceOf(new IsCovariantClass.LiftLiskov[IList] {

    override def liftLiskov[A, B](implicit ev: A <~< B): IList[A] <~< IList[B] = {
      type <~~<[F[_], G[_]] = ∀.Prototype[λ[α => F[α] <~< G[α]]]
      val ev1 = Λ[α](Maybe2.isCovariant_1[α].liftLiskov[A, B]): Maybe2[A, ?] <~~< Maybe2[B, ?]
      Fix.liftLiskov[Maybe2[A, ?], Maybe2[B, ?]](ev1.make)(Maybe2.isCovariant_2[A])
    }
  })
}
