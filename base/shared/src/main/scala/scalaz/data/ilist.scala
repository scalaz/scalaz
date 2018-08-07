package scalaz
package data

import Predef._
import prop._
import tc._
import Scalaz._

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

object IListModule {
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

  implicit final def ilistMonad: Monad[IList] =
    instanceOf(new MonadClass[IList] {
      def pure[A](a: A): IList[A] = IList.cons(a, IList.empty)
      def ap[A, B](fa: IList[A])(ff: IList[A => B]): IList[B] =
        flatten(map(fa)(a => map(ff)(f => f(a))))
      def flatMap[A, B](ma: IList[A])(f: A => IList[B]): IList[B] =
        flatten(map(ma)(f))
      def flatten[A](nested: IList[IList[A]]): IList[A] =
        nested.foldRight(IList.empty[A])((out, as) => as.append(out))
      def map[A, B](ma: IList[A])(f: A => B): IList[B] =
        ma.foldRight(IList.empty[B])((a, bs) => IList.cons(f(a), bs))
    })

  implicit final def ilistMonoid[A]: Monoid[IList[A]] =
    instanceOf(new MonoidClass[IList[A]] {
      def mempty: IList[A] = IList.empty[A]
      def mappend(l1: IList[A], l2: => IList[A]): IList[A] =
        l1.append(l2)
    })

  implicit final def ilistTraversable: Traversable[IList] =
    instanceOf(new TraversableClass[IList] {
      @tailrec
      def foldLeft[A, B](fa: IList[A], z: B)(f: (B, A) => B): B =
        IList.uncons(fa) match {
          case Maybe2.Empty2() => z
          case Maybe2.Just2(a, as) =>
            foldLeft(as, f(z, a))(f)
        }

      def foldMap[A, B](fa: IList[A])(f: A => B)(implicit B: scalaz.tc.Monoid[B]): B =
        foldLeft(fa, B.mempty)((b, a) => B.mappend(b, f(a)))

      def foldRight[A, B](fa: IList[A], z: => B)(f: (A, => B) => B): B =
        fa.reverse.foldLeft(z)((b, a) => f(a, b))

      def toList[A](fa: IList[A]): scala.List[A] = {
        import scala.{ ::, List,Nil }
        foldRight[A, List[A]](fa, Nil)(new ::(_, _))
      }

      def sequence[F[_], A](ta: IList[F[A]])(implicit F: Applicative[F]): F[IList[A]] =
        traverse(ta)(fa => fa)

      def traverse[F[_], A, B](ta: IList[A])(f: A => F[B])(implicit F: Applicative[F]): F[IList[B]] =
        ta.uncons match {
          case Maybe2.Empty2() =>
            F.pure(IList.empty[B])
          case Maybe2.Just2(a, as) =>
            F.ap(traverse(as)(f))(f(a).map(b => (ls: IList[B]) => IList.cons(b, ls)))
        }

      def map[A, B](ma: IList[A])(f: A => B): IList[B] =
        ma.foldRight(IList.empty[B])((a, bs) => IList.cons(f(a), bs))

    })

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

    def isEmpty: Boolean =
      IList.uncons(self) match {
        case Maybe2.Empty2() => true
        case _               => false
      }

    def nonEmpty: Boolean = !isEmpty

    def ::(a: A): IList[A] = IList.cons(a, self)

    def uncons: Maybe2[A, IList[A]] =
      IList.uncons(self)

    def append(that: IList[A]): IList[A] =
      that.reverse.foldLeft(self)((b, a) => IList.cons(a, b))

    def :::(that: IList[A]): IList[A] =
      self.append(that)

    def ++(that: IList[A]): IList[A] =
      that.append(self)

    def reverse: IList[A] =
      self.foldLeft(IList.empty[A])((b, a) => a :: b)

    def reverse_:::[Z](that: IList[A]): IList[A] =
      that.foldLeft(self)((as, a) => a :: as)

    def filter(p: A => Boolean): IList[A] =
      self.foldLeft(IList.empty[A])((b, a) => if (p(a)) a :: b else b)

    def exists(p: A => Boolean): Boolean =
      self.foldLeft(false)((b, a) => p(a) || b)

    def forall(p: A => Boolean): Boolean =
      self.foldLeft(true)((b, a) => p(a) && b)

    def find(p: A => Boolean): Maybe[A] = {
      @tailrec
      def go(as: IList[A]): Maybe[A] =
        IList.uncons(as) match {
          case Maybe2.Empty2() => Maybe.empty
          case Maybe2.Just2(a, aas) =>
            if (p(a)) Maybe.just(a)
            else go(aas)
        }
      go(self)
    }

    def index(n: Int): Maybe[A] = {
      @tailrec
      def go(m: Int, as: IList[A]): Maybe[A] =
        IList.uncons(as) match {
          case Maybe2.Empty2() => Maybe.empty
          case Maybe2.Just2(a, aas) =>
            if (m == n) Maybe.just(a)
            else go(m + 1, aas)
        }
      if (n < 0) Maybe.empty
      else go(0, self)
    }

    def !!(n: Int): Maybe[A] = self.index(n)

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

    def zipWithIndex: IList[(Int, A)] = {
      @tailrec
      def go(acc: IList[(Int, A)], m: Int, as: IList[A]): IList[(Int, A)] =
        IList.uncons(as) match {
          case Maybe2.Just2(a, aas) =>
            go((m, a) :: acc, m + 1, aas)
          case _ => acc.reverse
        }

      go(IList.empty, 0, self)
    }

    def take(n: Int): IList[A] = {
      @tailrec
      def go(m: Int, acc: IList[A], as: IList[A]): IList[A] =
        if (m == 0) acc.reverse
        else
          IList.uncons(as) match {
            case Maybe2.Empty2() => acc.reverse
            case Maybe2.Just2(a, aas) =>
              go(m - 1, a :: acc, aas)
          }

      if (n <= 0) IList.empty
      else
        go(n, IList.empty, self)
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

    def takeWhile(p: A => Boolean): IList[A] = {
      @tailrec
      def go(acc: IList[A], as: IList[A]): IList[A] =
        IList.uncons(as) match {
          case Maybe2.Empty2() => acc.reverse
          case Maybe2.Just2(a, aas) =>
            if (p(a)) go(a :: acc, aas)
            else acc.reverse
        }
      go(IList.empty, self)
    }

    def dropWhile(p: A => Boolean): IList[A] = {
      @tailrec
      def go(as: IList[A]): IList[A] =
        IList.uncons(as) match {
          case Maybe2.Empty2() => as
          case Maybe2.Just2(a, aas) =>
            if (p(a)) go(aas)
            else as
        }
      go(self)
    }

    def size: Int = {
      @tailrec
      def go(m: Int, as: IList[A]): Int =
        IList.uncons(as) match {
          case Maybe2.Empty2() => m
          case Maybe2.Just2(_, aas) =>
            go(m + 1, aas)
        }
      go(0, self)
    }
  }
}

private[data] object IListImpl extends IListModule {
  type IList[A] = Fix[Maybe2[A, ?]]

  def empty[A]: IList[A] =
    Fix.fix[Maybe2[A, ?]](Maybe2.empty2)

  def cons[A](a: A, as: Fix[Maybe2[A, ?]]): IList[A] =
    Fix.fix[Maybe2[A, ?]](Maybe2.just2(a, as))

  def uncons[A](as: IList[A]): Maybe2[A, IList[A]] =
    Fix.unfix[Maybe2[A, ?]](as)

  implicit val isCovariantInstance: IsCovariant[IList] = new IsCovariant.LiftLiskov[IList] {

    override def liftLiskov[A, B](implicit ev: A <~< B): IList[A] <~< IList[B] = {
      type <~~<[F[_], G[_]] = ∀.Prototype[λ[α => F[α] <~< G[α]]]
      val ev1 = Λ[α](Maybe2.isCovariant_1[α].liftLiskov[A, B]): Maybe2[A, ?] <~~< Maybe2[B, ?]
      Fix.liftLiskov[Maybe2[A, ?], Maybe2[B, ?]](ev1.make)(Maybe2.isCovariant_2[A])
    }
  }
}

trait IListFunctions {
  @inline def cons[A](a: A, as: IList[A]): IList[A] =
    IList.cons(a, as)

  @inline def uncons[A](as: IList[A]): Maybe2[A, IList[A]] =
    IList.uncons(as)
}
