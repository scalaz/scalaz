package scalaz

import annotation.tailrec

/**
 * Provides a pointed stream, which is a non-empty zipper-like stream structure that tracks an index (focus)
 * position in a stream. Focus can be moved forward and backwards through the stream, elements can be inserted
 * before or after the focused position, and the focused item can be deleted.
 * <p/>
 * Based on the pointedlist library by Jeff Wheeler.
 */
sealed trait Zipper[+A] {
  val focus: A
  val lefts: Stream[A]
  val rights: Stream[A]

  import Scalaz._

  /**
   * Get the Stream representation of this Zipper. This fully traverses `lefts`. `rights` is
   * not evaluated.
   */
  def toStream: Stream[A] =
    lefts.reverse ++ focus #:: rights

  /**
   * Possibly moves to next element to the right of focus.
   */
  def next: Option[Zipper[A]] = rights match {
    case Stream.Empty => None
    case r #:: rs => Some(zipper(Stream.cons(focus, lefts), r, rs))
  }

  /**
   * Moves to the next element to the right of focus, or error if there is no element on the right.
   */
  def tryNext: Zipper[A] = next err "cannot move to next element"

  /**
   * Possibly moves to the previous element to the left of focus.
   */
  def previous: Option[Zipper[A]] = lefts match {
    case Stream.Empty => None
    case l #:: ls => Some(zipper(ls, l, Stream.cons(focus, rights)))
  }

  /**
   * Moves to the previous element to the left of focus, or error if there is no element on the left.
   */
  def tryPrevious: Zipper[A] = previous err "cannot move to previous element"

  /**
   * An alias for insertRight
   */
  def insert[AA >: A]: (AA => Zipper[AA]) = insertRight(_: AA)

  /**
   * Inserts an element to the left of focus and focuses on the new element.
   */
  def insertLeft[AA >: A](y: AA): Zipper[AA] = zipper(lefts, y, focus #:: rights)

  /**
   * Inserts an element to the right of focus and focuses on the new element.
   */
  def insertRight[AA >: A](y: AA): Zipper[AA] = zipper(focus #:: lefts, y, rights)

  /**
   * An alias for deleteRight
   */
  def delete: Option[Zipper[A]] = deleteRight

  /**
   * Deletes the element at focus and moves the focus to the left. If there is no element on the left,
   * focus is moved to the right.
   */
  def deleteLeft: Option[Zipper[A]] = rights match {
    case Stream.Empty => None
    case r #:: rs => Some(lefts match {
      case Stream.Empty => zipper(Stream.Empty, r, rs)
      case l #:: ls => zipper(ls, l, rights)
    })
  }

  /**
   * Deletes the element at focus and moves the focus to the right. If there is no element on the right,
   * focus is moved to the left.
   */
  def deleteRight: Option[Zipper[A]] = rights match {
    case Stream.Empty => None
    case r #:: rs => Some(lefts match {
      case Stream.Empty => zipper(Stream.Empty, r, rs)
      case l #:: ls => zipper(ls, l, rights)
    })
  }

  /**
   * Deletes all elements except the focused element.
   */
  def deleteOthers: Zipper[A] = zipper(Stream.Empty, focus, Stream.Empty)

  def length: Int = this.foldr(0)(((a: Any, b: Int) => b + 1)(_, _))

  /**
   * Whether the focus is on the first element in the zipper.
   */
  def atStart: Boolean = lefts.isEmpty

  /**
   * Whether the focus is on the last element in the zipper.
   */
  def atEnd: Boolean = rights.isEmpty

  /**
   * Pairs each element with a boolean indicating whether that element has focus.
   */
  def withFocus: Zipper[(A, Boolean)] = zipper(lefts.zip(Stream.continually(false)), (focus, true), rights.zip(Stream.continually(false)))

  /**
   * Moves focus to the nth element of the zipper, or None if there is no such element.
   */
  def move(n: Int): Option[Zipper[A]] = {
    @tailrec
    def move0(z: Option[Zipper[A]], n: Int): Option[Zipper[A]] =
      if (n > 0 && rights.isEmpty || n < 0 && lefts.isEmpty) None
      else {
        if (n == 0) z
        else if (n > 0) move0(z >>= ((_:Zipper[A]).next), n - 1)
        else move0(z >>= ((_:Zipper[A]).previous), n + 1)
      }
    move0(Some(this), n)
  }

  /**
   * Moves focus to the nearest element matching the given predicate, preferring the left,
   * or None if no element matches.
   */
  def findZ(p: A => Boolean): Option[Zipper[A]] =
    if (p(focus)) Some(this)
    else {
      val c = this.positions
      c.lefts.merge(c.rights).find((x => p(x.focus)))
    }

  /**
   * Given a traversal function, find the first element along the traversal that matches a given predicate.
   */
  def findBy[AA >: A](f: Zipper[AA] => Option[Zipper[AA]])(p: AA => Boolean): Option[Zipper[AA]] = {
    f(this) ∗ (x => if (p(x.focus)) Some(x) else x.findBy(f)(p))
  }

  /**
   * Moves focus to the nearest element on the right that matches the given predicate,
   * or None if there is no such element.
   */
  def findNext[AA >: A](p: AA => Boolean): Option[Zipper[AA]]= findBy((z: Zipper[A]) => z.next)(p)

  /**
   * Moves focus to the previous element on the left that matches the given predicate,
   * or None if there is no such element.
   */
  def findPrevious[AA >: A](p: AA => Boolean): Option[Zipper[AA]]= findBy((z: Zipper[A]) => z.previous)(p)

  /**
   * A zipper of all positions of the zipper, with focus on the current position.
   */
  def positions: Zipper[Zipper[A]] = {
    val left = this.unfold[Stream, Zipper[A]]((p: Zipper[A]) => p.previous.map(x => (x, x)))
    val right = this.unfold[Stream, Zipper[A]]((p: Zipper[A]) => p.next.map(x => (x, x)))

    zipper(left, this, right)
  }

  /**
   * The index of the focus.
   */
  def index: Int = lefts.length

  /**
   * Moves focus to the next element. If the last element is currently focused, loop to the first element.
   */
  def nextC: Zipper[A] = (lefts, rights) match {
    case (Stream.Empty, Stream.Empty) => this
    case (_, Stream.Empty) => {
      val xs = lefts.reverse
      zipper(rights, xs.head, xs.tail.append(Stream(focus)))
    }
    case (_, _) => tryNext
  }

  /**
   * Moves focus to the previous element. If the first element is currently focused, loop to the last element.
   */
  def previousC: Zipper[A] = (lefts, rights) match {
    case (Stream.Empty, Stream.Empty) => this
    case (Stream.Empty, _) => {
      val xs = rights.reverse
      zipper(xs.tail.append(Stream(focus)), xs.head, lefts)
    }
    case (_, _) => tryPrevious
  }

  /**
   * Deletes the focused element and moves focus to the left. If the focus was on the first element,
   * focus is moved to the last element.
   */
  def deleteLeftC: Option[Zipper[A]] = rights match {
    case Stream.Empty => None
    case _ #:: _ => Some(lefts match {
      case l #:: ls => zipper(ls, l, rights)
      case Stream.Empty => {
        val r = rights.reverse
        zipper(r.tail, r.head, Stream.Empty)
      }
    })
  }

  /**
   * Deletes the focused element and moves focus to the right. If the focus was on the last element,
   * focus is moved to the first element.
   */
  def deleteRightC: Option[Zipper[A]] = lefts match {
    case Stream.Empty => None
    case _ #:: _ => Some(rights match {
      case r #:: rs => zipper(lefts, r, rs)
      case Stream.Empty => {
        val l = lefts.reverse
        zipper(Stream.Empty, l.head, l.tail)
      }
    })
  }

  /**
   * An alias for deleteRightC
   */
  def deleteC: Option[Zipper[A]] = deleteRightC
}

trait Zippers {
  def zipper[A](ls: Stream[A], a: A, rs: Stream[A]): Zipper[A] = new Zipper[A] {
    val focus = a
    val lefts = ls
    val rights = rs

    override def toString = "<zipper>"
  }
}
