package scalaz

import MA._

/**
 * Provides a pointed stream, which is a non-empty zipper-like stream structure that tracks an index (focus)
 * position in a stream. Focus can be moved forward and backwards through the stream, elements can be inserted
 * before or after the focused position, and the focused item can be deleted.
 * <p/>
 * Based on the pointedlist library by Jeff Wheeler.
 */

sealed trait Zipper[+A] extends Iterable[A] {
  val focus: A
  val lefts: Stream[A]
  val rights: Stream[A]

  def elements = (lefts.reverse ++ Stream.cons(focus, rights)).elements

  def next = rights match {
    case Stream.empty => None
    case rs => Some(tryNext)
  }

  def tryNext = rights match {
    case Stream.empty => error("cannot move to next element")
    case Stream.cons(r, rs) => zipper(Stream.cons(focus, lefts), r, rs)
  }

  def previous = lefts match {
    case Stream.empty => None
    case ls => Some(tryPrevious)
  }

  def tryPrevious = lefts match {
    case Stream.empty => error("cannot move to previous element")
    case Stream.cons(l, ls) => zipper(ls, l, Stream.cons(focus, rights))
  }

  def insert = insertRight

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

  def length = foldr(const(_ + 1)) 0
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