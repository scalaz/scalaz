package scalaz

/**
 * Provides a pointed stream, which is a non-empty zipper-like stream structure that tracks an index (focus)
 * position in a stream. Focus can be moved forward and backwards through the stream, elements can be inserted
 * before or after the focused position, and the focused item can be deleted.
 * <p/>
 * Based on the pointedlist library by Jeff Wheeler.
 */

sealed trait Zipper[A] {
  val focus: A
  val lefts: Stream[A]
  val rights: Stream[A]

  import Zipper._
  import Cojoin._
  import S._

  /**
   * Possibly moves to next element to the right of focus.
   */
  def next = rights match {
    case Stream.empty => None
    case Stream.cons(r, rs) => Some(zipper(Stream.cons(focus, lefts), r, rs))
  }

  /**
   * Moves to the next element to the right of focus, or error if there is no element on the right.
   */
  def tryNext = next err "cannot move to next element"

  /**
   * Possibly moves to the previous element to the left of focus.
   */
  def previous = lefts match {
    case Stream.empty => None
    case Stream.cons(l, ls) => Some(zipper(ls, l, Stream.cons(focus, rights)))
  }

  /**
   * Moves to the previous element to the left of focus, or error if there is no element on the left.
   */
  def tryPrevious = previous err "cannot move to previous element"

  /**
   * An alias for insertRight
   */
  def insert = insertRight(_)

  /**
   * Inserts an element to the left of focus and focuses on the new element.
   */
  def insertLeft(y: A) = zipper(lefts, y, Stream.cons(focus, rights))

  /**
   * Inserts an element to the right of focus and focuses on the new element.
   */
  def insertRight(y: A) = zipper(Stream.cons(focus, lefts), y, rights)

  /**
   * An alias for deleteRigth
   */
  def delete = deleteRight

  /**
   * Deletes the element at focus and moves the focus to the left. If there is no element on the left,
   * focus is moved to the right.
   */
  def deleteLeft = (lefts, rights) match {
    case (Stream.empty, Stream.empty) => None
    case (Stream.cons(l, ls), rs) => Some(zipper(ls, l, rs))
    case (Stream.empty, Stream.cons(r, rs)) => Some(zipper(Stream.empty, r, rs))
  }

  /**
   * Deletes the element at focus and moves the focus to the right. If there is no element on the right,
   * focus is moved to the left.
   */
  def deleteRight = (lefts, rights) match {
    case (Stream.empty, Stream.empty) => None
    case (Stream.cons(l, ls), rs) => Some(zipper(ls, l, rs))
    case (Stream.empty, Stream.cons(r, rs)) => Some(zipper(Stream.empty, r, rs))
  }

  /**
   * Deletes all elements except the focused element.
   */
  def deleteOthers = zipper(Stream.empty, focus, Stream.empty)

  def length = this.foldr[Int](0, ((a: A, b: Int) => b + 1)(_, _))

  /**
   * Whether the focus is on the first element in the zipper.
   */
  def atStart = lefts.isEmpty

  /**
   * Whether the focus is on the last element in the zipper.
   */
  def atEnd = rights.isEmpty

  /**
   * Pairs each element with a boolean indicating whether that element has focus.
   */
  def withFocus = zipper(lefts.zip(Stream.const(false)), (focus, true), rights.zip(Stream.const(false)))

  /**
   * Moves focus to the nth element of the zipper, or None if there is no such element.
   */
  def move(n: Int): Option[Zipper[A]] =
    if (n < 0 || n >= length) None
    else {
      val l = lefts.length
      if (l == n) Some(this)
      else if (l >= n) tryPrevious.move(n)
      else tryNext.move(n)
    }

  /**
   * Moves focus to the nearest element matching the given predicate, preferring the left,
   * or None if no element matches.
   */
  import StreamW._
  def findZ(p: A => Boolean): Option[Zipper[A]] =
    if (p(focus)) Some(this)
    else {
      val c = this.positions
      c.lefts.merge(c.rights).find((x => p(x.focus)))
    }

  /**
   * Given a traversal function, find the first element along the traversal that matches a given predicate. 
   */
  def findBy(f: Zipper[A] => Option[Zipper[A]])(p: A => Boolean): Option[Zipper[A]] = {
    f(this) >>= (x => if (p(x.focus)) Some(x) else x.findBy(f)(p))
  }

  /**
   * Moves focus to the nearest element on the right that matches the given predicate,
   * or None if there is no such element.
   */
  def findNext = findBy((z: Zipper[A]) => z.next)(_)

  /**
   * Moves focus to the previous element on the left that matches the given predicate,
   * or None if there is no such element. 
   */
  def findPrevious = findBy((z: Zipper[A]) => z.previous)(_)

  /**
   * A zipper of all positions of the zipper, with focus on the current position.
   */
  def positions: Zipper[Zipper[A]] = {
    val left = this.unfold[Stream]((p: Zipper[A]) => p.previous.map(x => (x, x)))
    val right = this.unfold[Stream]((p: Zipper[A]) => p.next.map(x => (x, x)))
    
    zipper(left, this, right)
  }

  /**
   * The index of the focus.
   */
  def index = lefts.length

  /**
   * Moves focus to the next element. If the last element is currently focused, loop to the first element.
   */
  def nextC = (lefts, rights) match {
    case (Stream.empty, Stream.empty) => this
    case (_, Stream.empty) => {
      val xs = lefts.reverse
      zipper(rights, xs.head, xs.tail.append(Stream(focus)))
    }
    case (_, _) => tryNext
  }

  /**
   * Moves focus to the previous element. If the first element is currently focused, loop to the last element.
   */
  def previousC = (lefts, rights) match {
    case (Stream.empty, Stream.empty) => this
    case (Stream.empty, _) => {
      val xs = rights.reverse
      zipper(xs.tail.append(Stream(focus)), xs.head, lefts)
    }
    case (_, _) => tryPrevious
  }

  /**
   * Deletes the focused element and moves focus to the left. If the focus was on the first element,
   * focus is moved to the last element.
   */
  def deleteLeftC = (lefts, rights) match {
    case (Stream.empty, Stream.empty) => None
    case (Stream.cons(l, ls), rs) => Some(zipper(ls, l, rs))
    case (Stream.empty, rs) => {
      val xs = rs.reverse
      Some(zipper(xs.tail, xs.head, Stream.empty))
    }
  }

  /**
   * Deletes the focused element and moves focus to the right. If the focus was on the last element,
   * focus is moved to the first element.
   */
  def deleteRightC = (lefts, rights) match {
    case (Stream.empty, Stream.empty) => None
    case (ls, Stream.cons(r, rs)) => Some(zipper(ls, r, rs))
    case (ls, Stream.empty) => {
      val xs = ls.reverse
      Some(zipper(Stream.empty, xs.head, xs.tail))
    }
  }

  /**
   * An alias for deleteRightC
   */
  def deleteC = deleteRightC
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
