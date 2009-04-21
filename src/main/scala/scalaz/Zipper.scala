package scalaz

/**
 * Provides a pointed stream, which is a non-empty zipper-like stream structure that tracks an index (focus)
 * position in a stream. Focus can be moved forward and backwards through the stream, elements can be inserted
 * before or after the focused position, and the focused item can be deleted.
 * <p/>
 * Based on the pointedlist library by Jeff Wheeler.
 */

sealed trait Zipper[A] extends Iterable[A] {
  val focus: A
  val lefts: Stream[A]
  val rights: Stream[A]

  def elements = (lefts.reverse ++ Stream.cons(focus, rights)).elements

  import Zipper._
  import MA._
  import S._

  def next = rights match {
    case Stream.empty => None
    case Stream.cons(r, rs) => Some(zipper(Stream.cons(focus, lefts), r, rs))
  }

  def tryNext = next err "cannot move to next element"

  def previous = lefts match {
    case Stream.empty => None
    case Stream.cons(l, ls) => Some(zipper(ls, l, Stream.cons(focus, rights)))
  }

  def tryPrevious = previous err "cannot move to previous element"

  def insert = insertRight(_)

  def insertLeft(y: A) = zipper(lefts, y, Stream.cons(focus, rights))

  def insertRight(y: A) = zipper(Stream.cons(focus, lefts), y, rights)

  def delete = deleteRight

  def deleteLeft = (lefts, rights) match {
    case (Stream.empty, Stream.empty) => None
    case (Stream.cons(l, ls), rs) => Some(zipper(ls, l, rs))
    case (Stream.empty, Stream.cons(r, rs)) => Some(zipper(Stream.empty, r, rs))
  }

  def deleteRight = (lefts, rights) match {
    case (Stream.empty, Stream.empty) => None
    case (Stream.cons(l, ls), rs) => Some(zipper(ls, l, rs))
    case (Stream.empty, Stream.cons(r, rs)) => Some(zipper(Stream.empty, r, rs))
  }

  def deleteOthers = zipper(Stream.empty, focus, Stream.empty)

  def length = this.foldr[Int](0, ((a: A, b: Int) => b + 1)(_, _))

  def atStart = lefts.isEmpty

  def atEnd = rights.isEmpty

  def withFocus = zipper(lefts.zip(Stream.const(false)), (focus, true), rights.zip(Stream.const(false)))

  def move(n: Int): Option[Zipper[A]] =
    if (n < 0 || n >= length) None
    else {
      val l = lefts.length
      if (l == n) Some(this)
      else if (l >= n) tryPrevious.move(n)
      else tryNext.move(n)
    }

  def findZ(p: A => Boolean): Option[Zipper[A]] =
    if (p(focus)) Some(this)
    else findPrevious(p) orElse findNext(p)

  def findBy(p: A => Boolean, f: (Zipper[A], (A => Boolean)) => Option[Zipper[A]]): Option[Zipper[A]] = {
    val x = f(this, p)
    if (x.isEmpty) None
    else if (p(x.get.focus)) x
    else x.get.findBy(p, f)
  }

  def findNext(p: A => Boolean) = findBy(p, (z: Zipper[A], f: A => Boolean) => z.next)

  def findPrevious(p: A => Boolean) = findBy(p, (z: Zipper[A], f: A => Boolean) => z.previous)
}

object Zipper {
  def zipper[A](ls: Stream[A], a: A, rs: Stream[A]) = new Zipper[A] {
    val focus = a
    val lefts = ls
    val rights = rs
  }

  def zipper[A](a: A) = new Zipper[A] {
    val focus = a
    val lefts = Stream.empty
    val rights = Stream.empty
  }

  def fromStream[A](s: Stream[A]) = s match {
    case Stream.empty => None
    case Stream.cons(h, t) => Some(zipper(Stream.empty, h, t))
  }

  def fromStreamEnd[A](s: Stream[A]) = s match {
    case Stream.empty => None
    case xs => {
      val xsp = xs.reverse
      Some(zipper(xsp.tail, xsp.head, Stream.empty))
    }
  }
}