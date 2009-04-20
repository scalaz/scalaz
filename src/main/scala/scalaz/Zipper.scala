package scalaz

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
}

object Zipper {
  def zipper[A](ls: Stream[A], a: A, rs: Stream[A]) = new Zipper[A] {
    val focus = a;
    val lefts = ls;
    val rights = rs;
  }

  def zipper[A](a: A) = new Zipper[A] {
    val focus = a;
    val lefts = Stream.empty
    val rights = Stream.empty
  }

  def fromStream[A](s: Stream[A]) = s match {
    case Stream.empty => None
    case Stream.cons(h, t) => Some(new Zipper[A] {
      val focus = h
      val lefts = Stream.empty
      val rights = t
    })
  }
}