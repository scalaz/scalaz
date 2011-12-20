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
trait DList[A] {
  import DList._
  def apply(xs: List[A]): Trampoline[List[A]]

  /** Convert to a normal list. */
  def toList: List[A] = apply(List()).run

  /** Prepend a single element in constant time. */
  def +:(a: A): DList[A] = mkDList(as => suspend(apply(as) map (a :: _)))

  /** Append a single element in constant time. */
  def :+(a: A): DList[A] = mkDList(as => suspend(apply(a :: as)))

  /** Append one list to another in constant time. */
  def ++(as: => DList[A]): DList[A] = 
    mkDList(xs => as(xs) >>= (ys => apply(ys)))

  /** List elimination of head and tail. */
  def uncons[B](z: => B, f: (A, DList[A]) => B): B =
    (apply(List()) >>= {
      case List() => return_(z)
      case x :: xs => return_(f(x, fromList(xs)))
    }).run

  /** Get the first element of the list. */
  def head: A = uncons(sys.error("DList.head: empty list"), (x, y) => x)

  /** Get the tail of the list. */
  def tail: DList[A] = uncons(sys.error("DList.tail: empty list"), (x, y) => y) 

  /** Fold over a difference list. */
  def foldr[B](z: => B)(f: (A, => B) => B): B = {
    def go(xs: DList[A], z: => B, f: (A, => B) => B): Trampoline[B] =
      suspend(xs.uncons(return_(z), (h, t) => go(t, z, f) map (x => f(h, x))))
    go(this, z, f).run
  }

  /** Map over a difference list. */
  def map[B](f: A => B): DList[B] =
    foldr(DList[B]())((x, y) => f(x) +: y)

  /** Map over a difference list, then flatten. */
  def flatMap[B](f: A => DList[B]) =
    foldr(DList[B]())((x, y) => f(x) ++ y)
}

object DList extends DListFunctions with DListInstances {
  def apply[A](xs: A*) = fromList(xs.toList)
}

trait DListInstances {
  implicit def dlistMonoid[A]: Monoid[DList[A]] = new Monoid[DList[A]] {
    val zero = DList[A]()
    def append(a: DList[A], b: => DList[A]) = a ++ b
  }
  implicit val dlistMonad: Monad[DList] = new MonadPlus[DList] {
    def point[A](a: => A) = DList(a)
    def bind[A, B](as: DList[A])(f: A => DList[B]) = as flatMap f
    def plus[A](a: DList[A], b: => DList[A]) = a ++ b
    def empty[A] = DList()
  }
  implicit def dlistEqual[A: Equal]: Equal[DList[A]] = {
    import std.list._
    Equal[List[A]].contramap((_: DList[A]).toList)
  }
}

trait DListFunctions {
  def mkDList[A](f: (=> List[A]) => Trampoline[List[A]]): DList[A] =
    new DList[A] {
      def apply(xs: List[A]) = f(xs)
    }
  def DL[A](f: (=> List[A]) => List[A]): DList[A] = mkDList(xs => return_(f(xs)))
  def fromList[A](as: => List[A]): DList[A] =
    DL(bs => as ++ bs)
  def concat[A](xs: List[DList[A]]): DList[A] =
    xs.foldRight(DList[A]())(_ ++ _)
  def replicate[A](n: Int, a: A): DList[A] =
    DL(xs => {
      def go(m: Int): List[A] = if (m <= 0) xs else a :: go(m - 1)           
      go(n)
    })
  def unfoldr[A, B](b: B, f: B => Option[(A, B)]): DList[A] = {
    def go(b: B, f: B => Option[(A, B)]): Trampoline[DList[A]] =
      f(b) map { case (a, c) => suspend(go(c, f)) map (a +: _) } getOrElse return_(DList())
    go(b, f).run
  }
}
