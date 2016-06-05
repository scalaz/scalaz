package scalaz

import Free._
import std.function._

/**
 * Difference lists: a data structure for `O(1)` append on lists.
 * Based on `Data.DList`, a Haskell library by Don Stewart.
 *
 * A difference list is a function that given a list, returns the
 * original contents of the difference list prepended at the given list.
 *
 * This structure supports `O(1)` append and snoc operations on lists,
 * making it very useful for append-heavy uses, such as logging and
 * pretty printing.
 */
final class DList[A] private[scalaz](f: IList[A] => Trampoline[IList[A]]) {
  import DList._
  def apply(xs: => IList[A]): Trampoline[IList[A]] = f(xs)

  /** Convert to an IList. */
  def toIList: IList[A] = apply(IList()).run

  /** Convert to a normal list. */
  def toList: List[A] = toIList.toList

  /** Prepend a single element in constant time. */
  def +:(a: A): DList[A] = mkDList(as => suspend(apply(as) map (a :: _)))

  /** Append a single element in constant time. */
  def :+(a: A): DList[A] = mkDList(as => suspend(apply(a :: as)))

  /** Append one list to another in constant time. */
  def ++(as: => DList[A]): DList[A] =
    mkDList(xs => suspend(as(xs) >>= (apply(_))))

  /** List elimination of head and tail. */
  def uncons[B](z: => B, f: (A, DList[A]) => B): B =
   (apply(IList()) >>= {
      case INil() => return_(z)
      case ICons(x, xs) =>
        val r = f(x, fromIList(xs))
        return_(r)
    }).run

  /** Get the first element of the list, if any. */
  def headOption: Option[A] = uncons(None, (x, _) => Some(x))

  /** Tests whether list is empty. */
  def isEmpty: Boolean = uncons(true, (_, _) => false)

  /** Get the tail of the list, if any. */
  def tailOption: Option[DList[A]] = uncons(None, (_, y) => Some(y))

  /** Fold over a difference list. */
  def foldr[B](z: => B)(f: (A, => B) => B): B =
    toIList.foldRight(z)((a,b) => f(a,b))

  /** Map over a difference list. */
  def map[B](f: A => B): DList[B] =
    DL(dl => (toIList.map(f)) ++ dl)

  /** Map over a difference list, then flatten. */
  def flatMap[B](f: A => DList[B]): DList[B] =
   foldr(DList[B]())((x, y) => f(x) ++ y)

  def zip[B](bs: => DList[B]): DList[(A,B)] = uncons(DList(), (h,t) => bs.uncons(DList(), (h2,t2) => (h → h2) +: (t zip t2)))
}

object DList extends DListInstances {
  def apply[A](xs: A*): DList[A] = fromIList(IList(xs: _*))

  def mkDList[A](f: (IList[A]) => Trampoline[IList[A]]): DList[A] =
    new DList[A](f)
  def DL[A](f: (=> IList[A]) => IList[A]): DList[A] = mkDList(xs => return_(f(xs)))

  def fromList[A](as: => List[A]): DList[A] =
    fromIList(IList.fromList(as))

  def fromIList[A](as: => IList[A]): DList[A] =
    DL(bs => as ++ bs)

  def concat[A](xs: IList[DList[A]]): DList[A] =
    xs.foldRight(DList[A]())(_ ++ _)

  def replicate[A](n: Int, a: A): DList[A] =
    DL(xs => {
      def go(m: Int): IList[A] = if (m <= 0) xs else a :: go(m - 1)
      go(n)
    })
  def unfoldr[A, B](b: B, f: B => Option[(A, B)]): DList[A] = {
    def go(b: B, f: B => Option[(A, B)]): Trampoline[DList[A]] =
      f(b) map { case (a, c) => suspend(go(c, f)) map (a +: _) } getOrElse return_(DList())
    go(b, f).run
  }
}

sealed abstract class DListInstances {
  implicit def dlistMonoid[A]: Monoid[DList[A]] = new Monoid[DList[A]] {
    val zero = DList[A]()
    def append(a: DList[A], b: => DList[A]) = a ++ b
  }
  implicit val dlistMonadPlus: MonadPlus[DList] with Traverse[DList] with BindRec[DList] with Zip[DList] with IsEmpty[DList] = new MonadPlus[DList] with Traverse[DList] with BindRec[DList] with Zip[DList] with IsEmpty[DList] {
    def point[A](a: => A) = DList(a)
    def bind[A, B](as: DList[A])(f: A => DList[B]) = as flatMap f
    def plus[A](a: DList[A], b: => DList[A]) = a ++ b
    def empty[A] = DList()
    def isEmpty[A](fa: DList[A]) = fa.isEmpty
    def zip[A,B](a: => DList[A], b: => DList[B]): DList[(A, B)] = a zip b
    def traverseImpl[F[_], A, B](fa: DList[A])(f: A => F[B])(implicit F: Applicative[F]): F[DList[B]] =
      fa.foldr(F.point(DList[B]()))((a, fbs) => F.apply2(f(a), fbs)(_ +: _))

    def tailrecM[A, B](f: A => DList[A \/ B])(a: A): DList[B] =
      DList.fromIList(BindRec[IList].tailrecM[A, B](f(_).toIList)(a))
  }
  implicit def dlistEqual[A: Equal]: Equal[DList[A]] = {
    import std.list._
    Equal[List[A]].contramap((_: DList[A]).toList)
  }

}
