package scalaz

import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance
import Scalaz._
import Liskov.{ <~<, refl }

/**
 * Safe, invariant alternative to stdlib `List`. Most methods on `List` have a sensible equivalent 
 * here, either on the `IList` interface itself or via typeclass instances (which are the same as
 * those defined for stdlib `List`). All methods are total and stack-safe.
 */
sealed abstract class IList[A] {

  // Operations, in alphabetic order

  def ++(as: IList[A]): IList[A] = 
    concat(as)

  def ++:(as: IList[A]): IList[A] = 
    as.concat(this)

  def +:(a: A): IList[A] = 
    ::(a)

  def /:[B](b: B)(f: (B, A) => B): B = 
    foldLeft(b)(f)

  def :+(a: A): IList[A] = 
    concat(IList(a))  

  def ::(a: A): IList[A] = 
    ICons(a, this) 

  def :::(as: IList[A]): IList[A] = 
    ++:(as)

  def :\[B](b: B)(f: (A, B) => B): B = 
    foldRight(b)(f)

  /** Returns `f` applied to contents if non-empty, otherwise the zero of `B`. */
  final def <^>[B](f: OneAnd[IList, A] => B)(implicit B: Monoid[B]): B = 
    uncons(B.zero, (h, t) => f(OneAnd(h, t)))

  def collect[B](pf: PartialFunction[A,B]): IList[B] = 
    flatMap(a => IList.fromOption(pf.lift(a)))

  def collectFirst[B](pf: PartialFunction[A,B]): Option[B] = 
    find(a => pf.lift(a).isDefined).map(pf)

  def concat(as: IList[A]): IList[A] = 
    foldRight(as)(_ :: _)

  // no contains; use Foldable#element

  def containsSlice(as: IList[A])(implicit ev: Equal[A]): Boolean = 
    indexOfSlice(as).isDefined

  def count(f: A => Boolean): Int = 
    foldLeft(0)((n, a) => if (f(a)) n + 1 else n)

  def drop(n: Int): IList[A] = {
    @tailrec def drop0(as: IList[A], n: Int): IList[A] =
      if (n < 1) as else as match {
        case INil() => INil()
        case ICons(_, t) => drop0(t, n - 1)
      }
    drop0(this, n)
  }

  def dropRight(n: Int): IList[A] = 
    reverse.drop(n).reverse

  def dropRightWhile(f: A => Boolean): IList[A] = 
    reverse.dropWhile(f).reverse

  def dropWhile(f: A => Boolean): IList[A] = {
    @tailrec def dropWhile0(as: IList[A]): IList[A] =
      as match {
        case ICons(h, t) if (f(h)) => dropWhile0(t)
        case a => a
      }
    dropWhile0(this)
  }

  def endsWith(as: IList[A])(implicit ev: Equal[A]): Boolean =
    reverse.startsWith(as.reverse)

  // no exists; use Foldable#any

  def filter(f: A => Boolean): IList[A] = 
    foldRight(IList.empty[A])((a, as) => if (f(a)) a :: as else as)

  def filterNot(f: A => Boolean): IList[A] = 
    filter(a => !f(a))

  def find(f: A => Boolean): Option[A] = {
    @tailrec def find0[A](as: IList[A])(f: A => Boolean): Option[A] =
      as match {
        case INil() => none
        case ICons(a, as) => if (f(a)) a.some else find0[A](as)(f)
      }
    find0(this)(f)
  }

  def flatMap[B](f: A => IList[B]): IList[B] = 
    foldRight(IList.empty[B])(f(_) ++ _)

  def flatten[B](implicit ev: A <~< IList[B]): IList[B] = 
    flatMap(a => ev(a))

  def foldLeft[B](b: B)(f: (B, A) => B): B = {
    @tailrec def foldLeft0[A,B](as: IList[A])(b: B)(f: (B, A) => B): B =
      as match {
        case INil() => b
        case ICons(a, as) => foldLeft0[A,B](as)(f(b, a))(f)
      }
    foldLeft0(this)(b)(f)
  }

  def foldRight[B](b: B)(f: (A, B) => B): B = 
    reverse.foldLeft(b)((b, a) => f(a, b))

  // no forall; use Foldable#all
  // no foreach; use Each#each if you dare

  def groupBy[K](f: A => K)(implicit ev: Order[K]): K ==>> IList[A] =
    foldLeft(==>>.empty[K, IList[A]]) { (m, a) => 
      m.alter(f(a), _.map(a :: _) orElse Some(IList(a)))
    } .map(_.reverse) // should we bother with this? we don't do it for groupBy1

  def groupBy1[K](f: A => K)(implicit ev: Order[K]): K ==>> OneAnd[IList, A] =
    foldLeft(==>>.empty[K, OneAnd[IList,A]]) { (m, a) => 
      m.alter(f(a), _.map(oa => OneAnd(a, oa.head :: oa.tail)) orElse Some(OneAnd(a, INil())))
    }

  def headOption: Option[A] = 
    uncons(None, (h, _) => Some(h))

  def indexOf(a: A)(implicit ev: Equal[A]): Option[Int] = 
    indexWhere(_ === a)
   
  def indexOfSlice(slice: IList[A])(implicit ev: Equal[A]): Option[Int] = {
    @tailrec def indexOfSlice0(i: Int, as: IList[A]): Option[Int] =
      if (as.startsWith(slice)) Some(i) else as match {
        case INil() => None
        case ICons(_, t) => indexOfSlice0(i + 1, t)
      }
    indexOfSlice0(0, this)
  }

  def indexWhere(f: A => Boolean): Option[Int] = {
    @tailrec def indexWhere0(i: Int, as: IList[A]): Option[Int] =
      as match {
        case INil() => None
        case ICons(h, t) => if (f(h)) Some(i) else indexWhere0(i + 1, t)
      }
    indexWhere0(0, this)
  }

  def initOption: Option[IList[A]] = 
    reverse.tailOption.map(_.reverse)

  def inits: IList[IList[A]] = 
    reverse.tails.map(_.reverse)

  def intersperse(a: A): IList[A] = {
    @tailrec def intersperse0(accum: IList[A], rest: IList[A]): IList[A] = rest match {
      case INil() => accum
      case ICons(x, INil()) => x :: accum
      case ICons(h, t) => intersperse0(a :: h :: accum, t)
    }
    intersperse0(INil(), this).reverse
  }

  def isEmpty: Boolean = 
    uncons(true, (_, _) => false)

  def lastIndexOf(a:A)(implicit ev: Equal[A]): Option[Int] =
    reverse.indexOf(a).map((length - 1) - _)

  def lastIndexOfSlice(as: IList[A])(implicit ev: Equal[A]): Option[Int] =
    reverse.indexOfSlice(as.reverse).map(length - _ - as.length)

  def lastIndexWhere(f: A => Boolean): Option[Int] =
    reverse.indexWhere(f).map((length - 1) - _)

  def lastOption: Option[A] = 
    reverse.headOption

  def length: Int = 
    foldLeft(0)((n, _) => n + 1)

  def map[B](f: A => B): IList[B] = 
    foldRight(IList.empty[B])(f(_) :: _)

  // private helper for mapAccumLeft/Right below
  private[this] def mapAccum[B, C](as: IList[A])(c: C, f: (C, A) => (C, B)): (C, IList[B]) =
    as.foldLeft((c, IList.empty[B])) { case ((c, bs), a) => f(c, a).rightMap(_ :: bs) }

  /** All of the `B`s, in order, and the final `C` acquired by a stateful left fold over `as`. */
  def mapAccumLeft[B, C](c: C, f: (C, A) => (C, B)): (C, IList[B]) =
    mapAccum(this)(c, f).rightMap(_.reverse)

  /** All of the `B`s, in order `as`-wise, and the final `C` acquired by a stateful right fold over `as`. */
  final def mapAccumRight[B, C](c: C, f: (C, A) => (C, B)): (C, IList[B]) =
    mapAccum(reverse)(c, f)

  // no min/max; use Foldable#minimum, maximum, etc.

  def nonEmpty: Boolean = 
    !isEmpty

  def padTo(n: Int, a: A): IList[A] = {
    @tailrec def padTo0(n: Int, init: IList[A], tail: IList[A]): IList[A] =
      if (n < 1) init.reverse ++ tail else tail match {
        case INil() => padTo0(n - 1, a :: init, INil())
        case ICons(h, t) => padTo0(n - 1, h :: init, t)
      }
    padTo0(n, INil(), this)
  }

  def partition(f: A => Boolean): (IList[A], IList[A]) =
    foldLeft((IList.empty[A], IList.empty[A])) {
      case ((ts, fs), a) => if (f(a)) (a :: ts, fs) else (ts, a :: fs)
    }.bimap(_.reverse, _.reverse)

  def patch(from: Int, patch: IList[A], replaced: Int): IList[A] = {
    val (init, tail) = splitAt(from)
    init ++ patch ++ (tail drop replaced)
  }

  def prefixLength(f: A => Boolean): Int = {
    @tailrec def prefixLength0(n: Int, as: IList[A]): Int =
      as match {
        case ICons(h, t) if (f(h)) => prefixLength0(n + 1, t)
        case _ => n
      }
    prefixLength0(0, this)
  }

  // no product, use Foldable#fold

  def reduceLeftOption(f: (A, A) => A): Option[A] = 
    uncons(None, (h, t) => Some(t.foldLeft(h)(f)))

  def reduceRightOption(f: (A, A) => A): Option[A] = 
    reverse.reduceLeftOption((a, b) => f(b, a)) 

  def reverse: IList[A] = 
    foldLeft(IList.empty[A])((as, a) => a :: as)

  def reverseMap[B](f: A => B): IList[B] =
    foldLeft(IList.empty[B])((bs, a) => f(a) :: bs)

  def reverse_:::(as: IList[A]): IList[A] =
    as.foldLeft(this)((as, a) => a :: as)

  def scanLeft[B](z: B)(f: (B, A) => B): IList[B] =
    reverse.scanRight(z)((a, b) => f(b, a)).reverse

  def scanRight[B](z: B)(f: (A, B) => B): IList[B] =
    foldRight((IList(z), z)) { case (a, (bs, b)) => 
      val b0 = f(a, b)
      (b0 :: bs, b0)
    }._1

  def slice(from: Int, until: Int): IList[A] =
    drop(from).take((until max 0)- (from max 0))

  def sortBy[B](f: A => B)(implicit B: Order[B]): IList[A] =     
    IList(toList.sortBy(f)(B.toScalaOrdering): _*)

  def sorted(implicit ev: Order[A]): IList[A] =
    sortBy(identity)

  def span(f: A => Boolean): (IList[A], IList[A]) = {
    @tailrec def span0(as: IList[A], accum: IList[A]): (IList[A], IList[A]) =
      as match {
        case INil() => (accum.reverse, INil())
        case ICons(h, t) => if (f(h)) span0(t, h :: accum) else (accum.reverse, as)
      }
    span0(this, INil())
  }

  def splitAt(n: Int): (IList[A], IList[A]) = {
    @tailrec def splitAt0(n: Int, as: IList[A], accum: IList[A]): (IList[A], IList[A]) =
      if (n < 1) (accum.reverse, as) else as match {
        case INil() => (accum.reverse, INil())
        case ICons(h, t) => splitAt0(n - 1, t, h :: accum)
      }
    splitAt0(n, this, INil())
  }

  def startsWith(as: IList[A])(implicit ev: Equal[A]): Boolean = {
    @tailrec def startsWith0(a: IList[A], b: IList[A]): Boolean =
      (a, b) match {
        case (_, INil()) => true
        case (ICons(ha, ta), ICons(hb, tb)) if ha === hb => startsWith0(ta, tb)
        case _ => false
      }
    startsWith0(this, as)
  }

  // no sum, use Foldable#fold

  def tails: IList[IList[A]] = {
    @tailrec def inits0(as: IList[A], accum: IList[IList[A]]): IList[IList[A]] =
      as match {
        case INil() => (as :: accum).reverse
        case ICons(_, t) => inits0(t, as :: accum)
      }
    inits0(this, INil())
  }

  def tailOption: Option[IList[A]] = 
    uncons(None, (_, t) => Some(t))

  def take(n: Int): IList[A] = {
    @tailrec def take0(n: Int, as: IList[A], accum: IList[A]): IList[A] =
      if (n < 1) accum else as match {
        case INil() => accum
        case ICons(h, t) => take0(n - 1, t, h :: accum)
      }
    take0(n, this, INil()).reverse
  }

  def takeRight(n: Int): IList[A] = 
    reverse.take(n).reverse

  def takeRightWhile(f: A => Boolean): IList[A] = 
    reverse.takeWhile(f).reverse

  def takeWhile(f: A => Boolean): IList[A] = {
    @tailrec def takeWhile0(as: IList[A], accum: IList[A]): IList[A] =
      as match {
        case ICons(h, t) if f(h) => takeWhile0(t, h :: accum)
        case _ => accum
      }
    takeWhile0(this, INil()).reverse
  }

  def toEphemeralStream: EphemeralStream[A] =
    uncons(EphemeralStream(), (h, t) => EphemeralStream.cons(h, t.toEphemeralStream))

  def toList: List[A] = 
    foldRight(Nil : List[A])(_ :: _)

  def toNel: Option[NonEmptyList[A]] =
    uncons(None, (h, t) => Some(NonEmptyList(h, t.toList: _*)))

  def toMap[K, V](implicit ev0: A <~< (K, V), ev1: Order[K]): K ==>> V =
    widen[(K,V)].foldLeft(==>>.empty[K,V])(_ + _)

  def toStream: Stream[A] =
    uncons(Stream.empty, (h, t) => h #:: t.toStream)

  override def toString: String =
    IList.show(Show.showA).shows(this) // lame, but helpful for debugging

  def toVector: Vector[A] = 
    foldRight(Vector[A]())(_ +: _)

  def toZipper: Option[Zipper[A]] =
    toStream.toZipper

  def uncons[B](n: => B, c: (A, IList[A]) => B): B =
    this match {
      case INil() => n
      case ICons(h, t) => c(h, t)
    }

  def unzip[B, C](implicit ev: A <~< (B, C)): (IList[B], IList[C]) =
    widen[(B,C)].foldLeft((IList.empty[B], IList.empty[C])) { 
      case ((as, bs), (a, b)) => (a :: as, b :: bs)
    }.bimap(_.reverse, _.reverse)

  /** Unlike stdlib's version, this is total and simply ignores indices that are out of range */
  def updated(index: Int, a: A): IList[A] = {
    @tailrec def updated0(n: Int, as: IList[A], accum: IList[A]): IList[A] =
      (n, as) match {
        case (0, ICons(h, t)) => accum.reverse ++ ICons(a, t)
        case (n, ICons(h, t)) => updated0(n - 1, t, h :: accum)
        case _ => accum.reverse
      }
    updated0(index, this, INil())
  }

  // many other zip variants; see Traverse#zip*

  def zip[B](b: => IList[B]): IList[(A, B)] = {
    @tailrec def zaccum(a: => IList[A], b: => IList[B], accum: IList[(A,B)]): IList[(A, B)] =
      (a, b) match {
        case (ICons(a, as), ICons(b, bs)) => zaccum(as, bs, (a, b) :: accum)
        case _ => accum
      }
    zaccum(this, b, INil()).reverse
  }

  // IList is invariant in behavior but covariant by nature, so we can safely widen to IList[B] 
  // given evidence that A is a subtype of B.
  def widen[B](implicit ev: A <~< B): IList[B] = 
    ev.subst[({type λ[-α] = IList[α @uncheckedVariance] <~< IList[B]})#λ](refl)(this)

  def zipWithIndex: IList[(A, Int)] = 
    zip(IList(0 until length : _*))

}

// In order to get exhaustiveness checking and a sane unapply in both 2.9 and 2.10 it seems
// that we need to use bare case classes. Sorry. Suggestions welcome.
case class INil[A]() extends IList[A]
case class ICons[A](head: A, tail: IList[A]) extends IList[A]

object IList extends IListFunctions with IListInstances {

  def apply[A](as: A*): IList[A] = 
    as.foldRight(empty[A])(ICons(_, _))

  def empty[A](): IList[A] =
    INil[A]()

  def fromList[A](as: List[A]): IList[A] = 
    as.foldRight(empty[A])(ICons(_, _))
  
  def fromOption[A](a: Option[A]): IList[A] = 
    a.cata(IList(_), IList.empty[A])

  def fill[A](n: Int)(a: A): IList[A] =
    INil().padTo(n, a)

}

trait IListFunctions

trait IListInstance0 {

  implicit def equal[A](implicit A0: Equal[A]): Equal[IList[A]] =
    new IListEqual[A] {
      val A = A0
    }

}

trait IListInstances extends IListInstance0 {

  implicit val instances =

    new Traverse[IList] with MonadPlus[IList] with Each[IList] with Zip[IList] with Unzip[IList] with Align[IList] with IsEmpty[IList] with Cobind[IList] {

      override def map[A, B](fa: IList[A])(f: A => B): IList[B] = 
        fa map f

      def point[A](a: => A): IList[A] = 
        IList(a)

      def bind[A, B](fa: IList[A])(f: A => IList[B]): IList[B] = 
        fa flatMap f

      def plus[A](a: IList[A],b: => IList[A]): IList[A] = 
        a ++ b

      def empty[A]: IList[A] = 
        IList.empty[A]

      def each[A](fa: IList[A])(f: A => Unit): Unit = 
        fa.foldLeft(()) { (_, a) => f(a); () }

      def zip[A, B](a: => IList[A], b: => IList[B]): IList[(A, B)] = 
        a zip b

      def isEmpty[A](fa: IList[A]): Boolean = 
        fa.headOption.isEmpty
      
      def cobind[A, B](fa: IList[A])(f: IList[A] => B) = 
        fa.uncons(INil(), (_, t) => f(fa) :: cobind(t)(f))

      override def cojoin[A](a: IList[A]) = 
        a.uncons(INil(), (_, t) => a :: cojoin(t))

      def traverseImpl[F[_], A, B](fa: IList[A])(f: A => F[B])(implicit F: Applicative[F]): F[IList[B]] =
        fa.foldRight(F.point(IList.empty[B]))((a, fbs) => F.apply2(f(a), fbs)(_ :: _))

      def unzip[A, B](a: IList[(A, B)]): (IList[A], IList[B]) =
        a.unzip

      def alignWith[A, B, C](f: A \&/ B => C): (IList[A], IList[B]) => IList[C] = {
        @tailrec def loop(aa: IList[A], bb: IList[B], accum: IList[C]): IList[C] = 
          (aa, bb) match {
            case (INil(), _) => accum.reverse ++ bb.map(b => f(\&/.That(b)))
            case (_, INil()) => accum.reverse ++ aa.map(a => f(\&/.This(a)))
            case (ICons(ah, at), ICons(bh, bt)) => loop(at, bt, f(\&/.Both(ah, bh)) :: accum)
          }
        (a, b) => loop(a, b, INil())
      }

    }


  implicit def order[A](implicit A0: Order[A]): Order[IList[A]] =
    new IListOrder[A] {
      val A = A0
    }

  implicit def monoid[A]: Monoid[IList[A]] =
    new Monoid[IList[A]] {      
      def append(f1: IList[A], f2: => IList[A]) = f1 ++ f2
      def zero: IList[A] = INil()
    }

  implicit def show[A](implicit A: Show[A]): Show[IList[A]] = 
    new Show[IList[A]] {
      override def show(as: IList[A]) = {
        @tailrec def commaSep(rest: IList[A], acc: Cord): Cord =
          rest match {
            case INil() => acc
            case ICons(x, xs) => commaSep(xs, (acc :+ ",") ++ A.show(x))
          }
        "[" +: (as match {
          case INil() => Cord()
          case ICons(x, xs) => commaSep(xs, A.show(x))
        }) :+ "]"
      }
    }

}


private trait IListEqual[A] extends Equal[IList[A]] {
  implicit def A: Equal[A]

  @tailrec final override def equal(a: IList[A], b: IList[A]): Boolean =
    (a, b) match {
      case (INil(), INil()) => true
      case (ICons(a, as), ICons(b, bs)) if a === b => equal(as, bs)
      case _ => false
    }

}

private trait IListOrder[A] extends Order[IList[A]] with IListEqual[A] {
  implicit def A: Order[A]

  import Ordering._

  @tailrec final def order(a1: IList[A], a2: IList[A]) =
    (a1, a2) match {
      case (INil(), INil()) => EQ
      case (INil(), ICons(_, _)) => LT
      case (ICons(_, _), INil()) => GT
      case (ICons(a, as), ICons(b, bs)) => 
        A.order(a, b) match {
          case EQ => order(as, bs)
          case x => x
        }
    }

}




