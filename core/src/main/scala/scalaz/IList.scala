package scalaz

import scala.annotation.tailrec
import scala.annotation.unchecked.uncheckedVariance
import std.option.{ cata, none, some }
import std.stream.{ toZipper => sToZipper }
import std.tuple.{ tuple2Bitraverse => BFT }
import Liskov.{ <~<, refl }
import IList.{empty, single}

/**
 * Safe, invariant alternative to stdlib `List`. Most methods on `List` have a sensible equivalent
 * here, either on the `IList` interface itself or via typeclass instances (which are the same as
 * those defined for stdlib `List`). All methods are total and stack-safe.
 */
sealed abstract class IList[A] extends Product with Serializable {

  // Operations, in alphabetic order

  /** alias for `concat` */
  def ++(as: IList[A]): IList[A] =
    concat(as)

  /** alias for `prepend` */
  def ++:(as: IList[A]): IList[A] =
    prepend(as)

  def +:(a: A): IList[A] =
    ::(a)

  /** alias for `foldLeft` */
  def /:[B](b: B)(f: (B, A) => B): B =
    foldLeft(b)(f)

  def :+(a: A): IList[A] =
    concat(single(a))

  def ::(a: A): IList[A] =
    ICons(a, this)

  def :::(as: IList[A]): IList[A] =
    ++:(as)

  /** alias for `foldRight` */
  def :\[B](b: B)(f: (A, B) => B): B =
    foldRight(b)(f)

  /** Returns `f` applied to contents if non-empty, otherwise the zero of `B`. */
  final def <^>[B](f: OneAnd[IList, A] => B)(implicit B: Monoid[B]): B =
    uncons(B.zero, (h, t) => f(OneAnd(h, t)))

  def collect[B](pf: PartialFunction[A,B]): IList[B] = {
    @tailrec def go(as: IList[A], acc: IList[B]): IList[B] =
      as match {
        case ICons(h, t) =>
          if(pf isDefinedAt h) go(t, ICons(pf(h), acc))
          else go(t, acc)
        case INil() => acc
      }
    go(this, empty).reverse
  }

  def collectFirst[B](pf: PartialFunction[A,B]): Option[B] =
    find(pf.isDefinedAt).map(pf)

  def concat(as: IList[A]): IList[A] =
    foldRight(as)(_ :: _)

  // no contains; use Foldable#element

  def containsSlice(as: IList[A])(implicit ev: Equal[A]): Boolean =
    indexOfSlice(as).isDefined

  def count(f: A => Boolean): Int =
    foldLeft(0)((n, a) => if (f(a)) n + 1 else n)

  def distinct(implicit A: Order[A]): IList[A] = {
    @tailrec def loop(src: IList[A], seen: ISet[A], acc: IList[A]): IList[A] =
      src match {
        case ICons(h, t) =>
          if(seen.notMember(h)){
            loop(t, seen.insert(h), h :: acc)
          }else{
            loop(t, seen, acc)
          }
        case INil() =>
          acc.reverse
      }
    loop(this, ISet.empty[A], empty[A])
  }

  def drop(n: Int): IList[A] = {
    @tailrec def drop0(as: IList[A], n: Int): IList[A] =
      if (n < 1) as else as match {
        case INil() => empty
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
        case ICons(a, as) => if (f(a)) some(a) else find0[A](as)(f)
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

  def groupBy[K](f: A => K)(implicit ev: Order[K]): K ==>> NonEmptyList[A] =
    foldLeft(==>>.empty[K, NonEmptyList[A]]) { (m, a) =>
      m.alter(f(a), _.map(a <:: _) orElse Some(NonEmptyList(a)))
    } .map(_.reverse) // should we bother with this? we don't do it for groupBy1

  def groupBy1[K](f: A => K)(implicit ev: Order[K]): K ==>> OneAnd[IList, A] =
    foldLeft(==>>.empty[K, OneAnd[IList,A]]) { (m, a) =>
      m.alter(f(a), _.map(oa => OneAnd(a, oa.head :: oa.tail)) orElse Some(OneAnd(a, empty)))
    }

  def headOption: Option[A] =
    uncons(None, (h, _) => Some(h))

  def headMaybe: Maybe[A] =
    uncons(Maybe.Empty(), (h, _) => Maybe.Just(h))

  def indexOf(a: A)(implicit ev: Equal[A]): Option[Int] =
    indexWhere(ev.equal(a, _))

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

  def interleave(that: IList[A]): IList[A] = {
    @tailrec def loop(xs: IList[A], ys: IList[A], acc: IList[A]): IList[A] = xs match {
      case ICons(h, t) =>
        loop(ys, t, h :: acc)
      case INil() =>
        acc reverse_::: ys
    }
    loop(this, that, IList.empty[A])
  }

  def intersperse(a: A): IList[A] = {
    @tailrec def intersperse0(accum: IList[A], rest: IList[A]): IList[A] = rest match {
      case INil() => accum
      case ICons(x, INil()) => x :: accum
      case ICons(h, t) => intersperse0(a :: h :: accum, t)
    }
    intersperse0(empty, this).reverse
  }

  def isEmpty: Boolean =
    uncons(true, (_, _) => false)

  def lastIndexOf(a:A)(implicit ev: Equal[A]): Option[Int] =
    reverse.indexOf(a).map((length - 1) - _)

  def lastIndexOfSlice(as: IList[A])(implicit ev: Equal[A]): Option[Int] =
    reverse.indexOfSlice(as.reverse).map(length - _ - as.length)

  def lastIndexWhere(f: A => Boolean): Option[Int] =
    reverse.indexWhere(f).map((length - 1) - _)

  @tailrec
  final def lastOption: Option[A] =
    this match {
      case ICons(a, INil()) => Some(a)
      case ICons(_, tail) => tail.lastOption
      case INil() => None
    }

  def length: Int =
    foldLeft(0)((n, _) => n + 1)

  def map[B](f: A => B): IList[B] = reverse.reverseMap(f)

  // private helper for mapAccumLeft/Right below
  private[this] def mapAccum[B, C](as: IList[A])(c: C, f: (C, A) => (C, B)): (C, IList[B]) =
    as.foldLeft((c, IList.empty[B])) { case ((c, bs), a) => BFT.rightMap(f(c, a))(_ :: bs) }

  /** All of the `B`s, in order, and the final `C` acquired by a stateful left fold over `as`. */
  def mapAccumLeft[B, C](c: C)(f: (C, A) => (C, B)): (C, IList[B]) =
    BFT.rightMap(mapAccum(this)(c, f))(_.reverse)

  /** All of the `B`s, in order `as`-wise, and the final `C` acquired by a stateful right fold over `as`. */
  final def mapAccumRight[B, C](c: C)(f: (C, A) => (C, B)): (C, IList[B]) =
    mapAccum(reverse)(c, f)

  // no min/max; use Foldable#minimum, maximum, etc.

  def nonEmpty: Boolean =
    !isEmpty

  def padTo(n: Int, a: A): IList[A] = {
    @tailrec def padTo0(n: Int, init: IList[A], tail: IList[A]): IList[A] =
      if (n < 1) init reverse_::: tail else tail match {
        case INil() => padTo0(n - 1, a :: init, empty)
        case ICons(h, t) => padTo0(n - 1, h :: init, t)
      }
    padTo0(n, empty, this)
  }

  def partition(f: A => Boolean): (IList[A], IList[A]) =
    BFT.umap(foldLeft((IList.empty[A], IList.empty[A])) {
      case ((ts, fs), a) => if (f(a)) (a :: ts, fs) else (ts, a :: fs)
    })(_.reverse)

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

  def prepend(as: IList[A]): IList[A] =
    as.concat(this)

  // no product, use Foldable#fold

  def reduceLeftOption(f: (A, A) => A): Option[A] =
    uncons(None, (h, t) => Some(t.foldLeft(h)(f)))

  def reduceRightOption(f: (A, A) => A): Option[A] =
    reverse.reduceLeftOption((a, b) => f(b, a))

  def reverse: IList[A] = {
    @tailrec def go(as: IList[A], acc: IList[A]): IList[A] = as match {
      case c : ICons[_] => go(c.tail, ICons(c.head, acc))
      case _ : INil[_] => acc
    }
    go(this, IList.empty)
  }

  def reverseMap[B](f: A => B): IList[B] = {
    @tailrec def go(as: IList[A], acc: IList[B]): IList[B] = as match {
      case c : ICons[_] => go(c.tail, ICons(f(c.head), acc))
      case _ : INil[_] => acc
    }
    go(this, IList.empty)
  }

  def reverse_:::(as: IList[A]): IList[A] =
    as.foldLeft(this)((as, a) => a :: as)

  private[this] def scan0[B](list: IList[A], z: B)(f: (B, A) => B): IList[B] = {
    @tailrec def go(as: IList[A], acc: IList[B], b: B): IList[B] =
      as match {
        case INil() => acc
        case ICons(h, t) =>
          val b0 = f(b, h)
          go(t, b0 :: acc, b0)
      }
    go(list, single(z), z)
  }

  def scanLeft[B](z: B)(f: (B, A) => B): IList[B] =
    scan0(this, z)(f).reverse

  def scanRight[B](z: B)(f: (A, B) => B): IList[B] =
    scan0(reverse, z)((b, a) => f(a, b))

  def slice(from: Int, until: Int): IList[A] =
    drop(from).take((until max 0)- (from max 0))

  def sortBy[B](f: A => B)(implicit B: Order[B]): IList[A] =
    IList(toList.sortBy(f)(B.toScalaOrdering): _*)

  def sorted(implicit ev: Order[A]): IList[A] =
    sortBy(identity)

  def span(f: A => Boolean): (IList[A], IList[A]) = {
    @tailrec def span0(as: IList[A], accum: IList[A]): (IList[A], IList[A]) =
      as match {
        case INil() => (this, empty)
        case ICons(h, t) => if (f(h)) span0(t, h :: accum) else (accum.reverse, as)
      }
    span0(this, empty)
  }

  def splitAt(n: Int): (IList[A], IList[A]) = {
    @tailrec def splitAt0(n: Int, as: IList[A], accum: IList[A]): (IList[A], IList[A]) =
      if (n < 1) (accum.reverse, as) else as match {
        case INil() => (this, empty)
        case ICons(h, t) => splitAt0(n - 1, t, h :: accum)
      }
    splitAt0(n, this, empty)
  }

  def startsWith(as: IList[A])(implicit ev: Equal[A]): Boolean = {
    @tailrec def startsWith0(a: IList[A], b: IList[A]): Boolean =
      (a, b) match {
        case (_, INil()) => true
        case (ICons(ha, ta), ICons(hb, tb)) if ev.equal(ha, hb) => startsWith0(ta, tb)
        case _ => false
      }
    startsWith0(this, as)
  }

  // no sum, use Foldable#fold

  def tails: IList[IList[A]] = {
    @tailrec def tails0(as: IList[A], accum: IList[IList[A]]): IList[IList[A]] =
      as match {
        case INil() => (as :: accum).reverse
        case ICons(_, t) => tails0(t, as :: accum)
      }
    tails0(this, empty)
  }

  def tailOption: Option[IList[A]] =
    uncons(None, (_, t) => Some(t))

  def take(n: Int): IList[A] = {
    @tailrec def take0(n: Int, as: IList[A], accum: IList[A]): IList[A] =
      if (n < 1) accum.reverse else as match {
        case ICons(h, t) => take0(n - 1, t, h :: accum)
        case INil() => this
      }
    take0(n, this, empty)
  }

  def takeRight(n: Int): IList[A] =
    drop(length - n)

  def takeRightWhile(f: A => Boolean): IList[A] = {
    @tailrec def go(as: IList[A], accum: IList[A]): IList[A] =
      as match {
        case ICons(h, t) if f(h) => go(t, h :: accum)
        case _ => accum
      }
    go(this.reverse, empty)
  }

  def takeWhile(f: A => Boolean): IList[A] = {
    @tailrec def takeWhile0(as: IList[A], accum: IList[A]): IList[A] =
      as match {
        case ICons(h, t) if f(h) => takeWhile0(t, h :: accum)
        case INil() => this
        case _ => accum.reverse
      }
    takeWhile0(this, empty)
  }

  def toEphemeralStream: EphemeralStream[A] =
    uncons(EphemeralStream(), (h, t) => EphemeralStream.cons(h, t.toEphemeralStream))

  def toList: List[A] =
    foldRight(Nil : List[A])(_ :: _)

  def toNel: Option[NonEmptyList[A]] =
    uncons(None, (h, t) => Some(NonEmptyList.nel(h, t)))

  def toMap[K, V](implicit ev0: A <~< (K, V), ev1: Order[K]): K ==>> V =
    widen[(K,V)].foldLeft(==>>.empty[K,V])(_ + _)

  def toStream: Stream[A] =
    uncons(Stream.empty, (h, t) => h #:: t.toStream)

  override def toString: String =
    IList.show(Show.showA).shows(this) // lame, but helpful for debugging

  def toVector: Vector[A] =
    foldRight(Vector[A]())(_ +: _)

  def toZipper: Option[Zipper[A]] =
    sToZipper(toStream)

  /**
   * Referentially transparent replacement for traverse, specialised to
   * disjunction.
   */
  def traverseDisjunction[E, B](f: A => E \/ B): E \/ IList[B] = {
    @tailrec def go(lst: IList[A], acc: IList[B]): E \/ IList[B] = lst match {
      case INil() => \/-(acc)
      case ICons(head, tail) =>
        f(head) match {
          case \/-(b)       => go(tail, b :: acc)
          case err @ -\/(_) => err
        }
    }
    go(this, IList.empty).map(_.reverse)
  }

  def uncons[B](n: => B, c: (A, IList[A]) => B): B =
    this match {
      case INil() => n
      case ICons(h, t) => c(h, t)
    }

  def unzip[B, C](implicit ev: A <~< (B, C)): (IList[B], IList[C]) =
    BFT.bimap(widen[(B,C)].foldLeft((IList.empty[B], IList.empty[C])) {
      case ((as, bs), (a, b)) => (a :: as, b :: bs)
    })(_.reverse, _.reverse)

  /** Unlike stdlib's version, this is total and simply ignores indices that are out of range */
  def updated(index: Int, a: A): IList[A] = {
    @tailrec def updated0(n: Int, as: IList[A], accum: IList[A]): IList[A] =
      (n, as) match {
        case (0, ICons(h, t)) => accum reverse_::: ICons(a, t)
        case (n, ICons(h, t)) => updated0(n - 1, t, h :: accum)
        case _ => this
      }
    updated0(index, this, empty)
  }

  // many other zip variants; see Traverse#zip*

  def zip[B](b: => IList[B]): IList[(A, B)] = {
    @tailrec def zaccum(a: IList[A], b: IList[B], accum: IList[(A,B)]): IList[(A, B)] =
      (a, b) match {
        case (ICons(a, as), ICons(b, bs)) => zaccum(as, bs, (a, b) :: accum)
        case _ => accum
      }
    if(this.isEmpty) empty
    else zaccum(this, b, empty).reverse
  }

  // IList is invariant in behavior but covariant by nature, so we can safely widen to IList[B]
  // given evidence that A is a subtype of B.
  def widen[B](implicit ev: A <~< B): IList[B] =
    this.asInstanceOf[IList[B]]

  def zipWithIndex: IList[(A, Int)] =
    zip(IList(0 until length : _*))

}

// In order to get exhaustiveness checking and a sane unapply in both 2.9 and 2.10 it seems
// that we need to use bare case classes. Sorry. Suggestions welcome.
final case class INil[A]() extends IList[A]
final case class ICons[A](head: A, tail: IList[A]) extends IList[A]

object IList extends IListInstances {
  private[this] val nil: IList[Nothing] = INil()

  // optimised versions of apply(A*)
  @inline final def apply[A](a: A, b: A): IList[A] = a :: single(b)
  @inline final def apply[A](a: A, b: A, c: A): IList[A] = a :: apply(b, c)
  @inline final def apply[A](a: A, b: A, c: A, d: A): IList[A] = a :: apply(b, c, d)
  @inline final def apply[A](a: A, b: A, c: A, d: A, e: A): IList[A] = a :: apply(b, c, d, e)
  @inline final def apply[A](a: A, b: A, c: A, d: A, e: A, f: A): IList[A] = a :: apply(b, c, d, e, f)

  def apply[A](as: A*): IList[A] =
    as.foldRight(empty[A])(ICons(_, _))

  @inline def single[A](a: A): IList[A] =
    ICons(a, empty)

  def empty[A]: IList[A] =
    nil.asInstanceOf[IList[A]]

  def fromList[A](as: List[A]): IList[A] =
    as.foldRight(empty[A])(ICons(_, _))

  def fromFoldable[F[_]: Foldable, A](as: F[A]): IList[A] =
    Foldable[F].foldRight(as, empty[A])(ICons(_, _))

  def fromOption[A](a: Option[A]): IList[A] =
    cata(a)(single, IList.empty[A])

  def fill[A](n: Int)(a: A): IList[A] = {
    @tailrec def go(i: Int, list: IList[A]): IList[A] = {
      if(i > 0) go(i - 1, ICons(a, list))
      else list
    }
    if(n <= 0) empty
    else go(n, empty)
  }

  import Isomorphism._

  val listIListIso: List <~> IList =
    new IsoFunctorTemplate[List, IList] {
      def to[A](fa: List[A]) = fromList(fa)
      def from[A](fa: IList[A]) = fa.toList
    }
}

sealed abstract class IListInstance0 {

  implicit def equal[A](implicit A0: Equal[A]): Equal[IList[A]] =
    new IListEqual[A] {
      val A = A0
    }

}

sealed abstract class IListInstances extends IListInstance0 {

  implicit val instances: Traverse[IList] with MonadPlus[IList] with BindRec[IList] with Zip[IList] with Unzip[IList] with Align[IList] with IsEmpty[IList] with Cobind[IList] =

    new Traverse[IList] with MonadPlus[IList] with BindRec[IList] with Zip[IList] with Unzip[IList] with Align[IList] with IsEmpty[IList] with Cobind[IList] {
      override def findLeft[A](fa: IList[A])(f: A => Boolean) =
        fa.find(f)

      override def findRight[A](fa: IList[A])(f: A => Boolean) = {
        @tailrec def loop(a: IList[A], x: Option[A]): Option[A] =
          a match {
            case ICons(h, t) =>
              loop(t, if(f(h)) Some(h) else x)
            case INil() =>
              x
          }
        loop(fa, None)
      }

      override def map[A, B](fa: IList[A])(f: A => B): IList[B] =
        fa map f

      def point[A](a: => A): IList[A] =
        single(a)

      def bind[A, B](fa: IList[A])(f: A => IList[B]): IList[B] =
        fa flatMap f

      def plus[A](a: IList[A],b: => IList[A]): IList[A] =
        a ++ b

      def empty[A]: IList[A] =
        IList.empty[A]

      def zip[A, B](a: => IList[A], b: => IList[B]): IList[(A, B)] =
        a zip b

      def isEmpty[A](fa: IList[A]): Boolean =
        fa.isEmpty

      override def empty[A](fa: IList[A]) =
        fa.isEmpty

      def cobind[A, B](fa: IList[A])(f: IList[A] => B) =
        fa.uncons(empty, (_, t) => f(fa) :: cobind(t)(f))

      override def cojoin[A](a: IList[A]) =
        a.uncons(empty, (_, t) => a :: cojoin(t))

      def traverseImpl[F[_], A, B](fa: IList[A])(f: A => F[B])(implicit F: Applicative[F]): F[IList[B]] =
        fa.foldRight(F.point(IList.empty[B]))((a, fbs) => F.apply2(f(a), fbs)(_ :: _))

      def unzip[A, B](a: IList[(A, B)]): (IList[A], IList[B]) =
        a.unzip

      def alignWith[A, B, C](f: A \&/ B => C): (IList[A], IList[B]) => IList[C] = {
        @tailrec def loop(aa: IList[A], bb: IList[B], accum: IList[C]): IList[C] =
          (aa, bb) match {
            case (INil(), _) => accum reverse_::: bb.map(b => f(\&/.That(b)))
            case (_, INil()) => accum reverse_::: aa.map(a => f(\&/.This(a)))
            case (ICons(ah, at), ICons(bh, bt)) => loop(at, bt, f(\&/.Both(ah, bh)) :: accum)
          }
        (a, b) => loop(a, b, empty)
      }

      override def toIList[A](fa: IList[A]) = fa

      override def foldLeft[A, B](fa: IList[A], z: B)(f: (B, A) => B) =
        fa.foldLeft(z)(f)

      override def foldRight[A, B](fa: IList[A], z: => B)(f: (A, => B) => B) =
        fa.foldRight(z)((a, b) => f(a, b))

      override def foldMapRight1Opt[A, B](fa: IList[A])(z: A => B)(f: (A, => B) => B) =
        foldMapLeft1Opt(fa.reverse)(z)((b, a) => f(a, b))

      override def foldMap[A, B](fa: IList[A])(f: A => B)(implicit M: Monoid[B]) =
        fa.foldLeft(M.zero)((b, a) => M.append(b, f(a)))

      override def foldMap1Opt[A, B](fa: IList[A])(f: A => B)(implicit M: Semigroup[B]) =
        fa match {
          case ICons(h, t) => Some(t.foldLeft(f(h))((b, a) => M.append(b, f(a))))
          case INil() => None
        }

      override def foldMapLeft1Opt[A, B](fa: IList[A])(z: A => B)(f: (B, A) => B) =
        fa match {
          case ICons(h, t) => Some(t.foldLeft(z(h))(f))
          case INil() => None
        }

      override def index[A](fa: IList[A], i: Int) = {
        @tailrec def go(as: IList[A], n: Int): Option[A] = as match {
          case ICons(h, t) => if(n == i) Some(h) else go(t, n + 1)
          case INil() => None
        }
        if(i < 0) None
        else go(fa, 0)
      }

      override def reverse[A](fa: IList[A]) =
        fa.reverse

      override def mapAccumL[S, A, B](fa: IList[A], z: S)(f: (S, A) => (S, B)) =
        fa.mapAccumLeft(z)(f)

      override def mapAccumR[S, A, B](fa: IList[A], z: S)(f: (S, A) => (S, B)) =
        fa.mapAccumRight(z)(f)

      override def any[A](fa: IList[A])(p: A => Boolean): Boolean = {
        @tailrec def loop(fa: IList[A]): Boolean = fa match {
          case INil() => false
          case ICons(h, t) => p(h) || loop(t)
        }
        loop(fa)
      }

      override def all[A](fa: IList[A])(p: A => Boolean): Boolean = {
        @tailrec def loop(fa: IList[A]): Boolean = fa match {
          case INil() => true
          case ICons(h, t) => p(h) && loop(t)
        }
        loop(fa)
      }

      override def widen[A, B](fa: IList[A])(implicit ev: A <~< B): IList[B] =
        fa.widen[B]

      def tailrecM[A, B](f: A => IList[A \/ B])(a: A): IList[B] = {
        @tailrec
        def go(xs: IList[IList[A \/ B]], bs: IList[B]): IList[B] =
          xs match {
            case ICons(ICons(-\/(a0), tail), rest) => go(ICons(f(a0), ICons(tail, rest)), bs)
            case ICons(ICons(\/-(b), tail), rest) => go(ICons(tail, rest), b :: bs)
            case ICons(INil(), rest) => go(rest, bs)
            case INil() => bs.reverse
          }
        go(IList(f(a)), INil())
      }
    }


  implicit def order[A](implicit A0: Order[A]): Order[IList[A]] =
    new IListOrder[A] {
      val A = A0
    }

  implicit def monoid[A]: Monoid[IList[A]] =
    new Monoid[IList[A]] {
      def append(f1: IList[A], f2: => IList[A]) = f1 ++ f2
      def zero: IList[A] = empty
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
    (a eq b) || {
      a match {
        case ac: ICons[A] => b match {
          case bc: ICons[A] =>
            ((ac.head.asInstanceOf[AnyRef] eq bc.head.asInstanceOf[AnyRef])
               || A.equal(ac.head, bc.head)) &&
            equal(ac.tail, bc.tail)
          case _ => false
        }
        case _: INil[A] => b match {
          case _ : INil[A] => true
          case _ => false
        }
      }
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
