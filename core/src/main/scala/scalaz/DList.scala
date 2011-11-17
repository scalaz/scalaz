package scalaz

import Free._
import std.function._

trait DList[A] {
  import DList._
  def apply(xs: List[A]): Trampoline[List[A]]
  def toList: List[A] = apply(List()).run
  def +:(a: A): DList[A] = mkDList(as => suspend(apply(as) map (a :: _)))
  def :+(a: A): DList[A] = mkDList(as => suspend(apply(a :: as)))
  def ++(as: => DList[A]): DList[A] = 
    mkDList(xs => as(xs) >>= (ys => apply(ys)))
  def uncons[B](z: => B, f: (A, DList[A]) => B): B =
    (apply(List()) >>= {
      case List() => return_(z)
      case x :: xs => return_(f(x, fromList(xs)))
    }).run
  def head: A = uncons(sys.error("DList.head: empty list"), (x, y) => x)
  def tail: DList[A] = uncons(sys.error("DList.tail: empty list"), (x, y) => y) 
  def foldr[B](z: => B)(f: (A, => B) => B): B = {
    def go(xs: DList[A], z: => B, f: (A, => B) => B): Trampoline[B] =
      suspend(xs.uncons(return_(z), (h, t) => go(t, z, f) map (x => f(h, x))))
    go(this, z, f).run
  }
  def map[B](f: A => B): DList[B] =
    foldr(DList[B]())((x, y) => f(x) +: y)
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
