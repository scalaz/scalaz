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

  def copy[AA >: A](lefts: Stream[AA] = this.lefts, focus: AA = this.focus, rights: Stream[AA] = this.rights): Zipper[AA] =
    Zipper(lefts, focus, rights)

  private def mergeStreams[T](s1: Stream[T], s2: Stream[T]): Stream[T] =
    if (s1.isEmpty) s2
    else s1.head #:: mergeStreams(s2, s1.tail)

  private def unfoldStream[T, B](x: T, f: T => Option[(B, T)]): Stream[B] =
    f(x) match {
      case None         => Stream()
      case Some((b, a)) => b #:: unfoldStream(a, f)
    }

  import Zipper._

  def map[B](f: A => B): Zipper[B] =
    zipper(lefts map f, f(focus), rights map f)

  /**
   * Get the Stream representation of this Zipper. This fully traverses `lefts`. `rights` is
   * not evaluated.
   */
  def toStream: Stream[A] =
    lefts.reverse ++ focus #:: rights

  /**
   * Update the focus in this zipper.
   */
  def update[AA >: A](focus: AA) = {
    this.copy(this.lefts, focus, this.rights)
  }

  /**
   * Apply f to the focus and update with the result.
   */
  def modify[AA >: A](f: A => AA) = this.update(f(this.focus))

  /**
   * Possibly moves to next element to the right of focus.
   */
  def next: Option[Zipper[A]] = rights match {
    case Stream.Empty => None
    case r #:: rs     => Some(zipper(Stream.cons(focus, lefts), r, rs))
  }

  /**
   * Possibly moves to next element to the right of focus.
   */
  def nextOr[AA >: A](z: => Zipper[AA]): Zipper[AA] =
    next getOrElse z

  /**
   * Moves to the next element to the right of focus, or error if there is no element on the right.
   */
  def tryNext: Zipper[A] = nextOr(sys.error("cannot move to next element"))

  /**
   * Possibly moves to the previous element to the left of focus.
   */
  def previous: Option[Zipper[A]] = lefts match {
    case Stream.Empty => None
    case l #:: ls     => Some(zipper(ls, l, Stream.cons(focus, rights)))
  }

  /**
   * Possibly moves to previous element to the left of focus.
   */
  def previousOr[AA >: A](z: => Zipper[AA]): Zipper[AA] =
    previous getOrElse z

  /**
   * Moves to the previous element to the left of focus, or error if there is no element on the left.
   */
  def tryPrevious: Zipper[A] = previousOr(sys.error("cannot move to previous element"))

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
   * An alias for `deleteRight`
   */
  def delete: Option[Zipper[A]] = deleteRight

  /**
   * Deletes the element at focus and moves the focus to the left. If there is no element on the left,
   * focus is moved to the right.
   */
  def deleteLeft: Option[Zipper[A]] = lefts match {
    case l #:: ls     => Some(zipper(ls, l, rights))
    case Stream.Empty => rights match {
      case r #:: rs     => Some(zipper(Stream.empty, r, rs))
      case Stream.Empty => None
    }
  }

  /**
   * Deletes the element at focus and moves the focus to the left. If there is no element on the left,
   * focus is moved to the right.
   */
  def deleteLeftOr[AA >: A](z: => Zipper[AA]): Zipper[AA] =
    deleteLeft getOrElse z

  /**
   * Deletes the element at focus and moves the focus to the right. If there is no element on the right,
   * focus is moved to the left.
   */
  def deleteRight: Option[Zipper[A]] = rights match {
    case r #:: rs     => Some(zipper(lefts, r, rs))
    case Stream.Empty => lefts match {
      case l #:: ls     => Some(zipper(ls, l, Stream.empty))
      case Stream.Empty => None
    }
  }

  /**
   * Deletes the element at focus and moves the focus to the right. If there is no element on the right,
   * focus is moved to the left.
   */
  def deleteRightOr[AA >: A](z: => Zipper[AA]): Zipper[AA] =
    deleteRight getOrElse z

  /**
   * Deletes all elements except the focused element.
   */
  def deleteOthers: Zipper[A] = zipper(Stream.Empty, focus, Stream.Empty)

  def foldLeft[B](b: B)(f: (B, A) => B): B =
    lefts.foldRight((focus #:: rights).foldLeft(b)((b, a) => f(b, a)))((a, b) => f(b, a))

  def foldRight[B](b: => B)(f: (A, => B) => B): B =
    lefts.foldLeft(Stream.cons(focus, rights).foldRight(b)((a, b) => f(a, b)))((a, b) => f(b, a))

  def length: Int =
    this.foldLeft(0)((b, _) => b + 1)

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
   * Moves focus n elements in the zipper, or None if there is no such element.
   *
   * @param  n  number of elements to move (positive is forward, negative is backwards)
   */
  def move(n: Int): Option[Zipper[A]] = {
    @tailrec
    def move0(z: Option[Zipper[A]], n: Int): Option[Zipper[A]] =
      if (n > 0 && rights.isEmpty || n < 0 && lefts.isEmpty) None
      else {
        if (n == 0) z
        else if (n > 0) move0(z flatMap ((_: Zipper[A]).next), n - 1)
        else move0(z flatMap ((_: Zipper[A]).previous), n + 1)
      }
    move0(Some(this), n)
  }

  /**
   * Moves focus to the start of the zipper.
   */
  def start: Zipper[A] = {
    val rights = this.lefts.reverse ++ focus #:: this.rights
    this.copy(Stream.Empty, rights.head, rights.tail)
  }

  /**
   * Moves focus to the end of the zipper.
   */
  def end: Zipper[A] = {
    val lefts = this.rights.reverse ++ focus #:: this.lefts
    this.copy(lefts.tail, lefts.head, Stream.empty)
  }

  /**
   * Moves focus to the nth element of the zipper, or the default if there is no such element.
   */
  def moveOr[AA >: A](n: Int, z: => Zipper[AA]): Zipper[AA] =
    move(n) getOrElse z

  /**
   * Moves focus to the nearest element matching the given predicate, preferring the left,
   * or None if no element matches.
   */
  def findZ(p: A => Boolean): Option[Zipper[A]] =
    if (p(focus)) Some(this)
    else {
      val c = this.positions
      mergeStreams(c.lefts, c.rights).find((x => p(x.focus)))
    }

  /**
   * Moves focus to the nearest element matching the given predicate, preferring the left,
   * or the default if no element matches.
   */
  def findZor[AA >: A](p: A => Boolean, z: => Zipper[AA]): Zipper[AA] =
    findZ(p) getOrElse z

  /**
   * Given a traversal function, find the first element along the traversal that matches a given predicate.
   */
  def findBy[AA >: A](f: Zipper[AA] => Option[Zipper[AA]])(p: AA => Boolean): Option[Zipper[AA]] = {
    f(this) flatMap (x => if (p(x.focus)) Some(x) else x.findBy(f)(p))
  }

  /**
   * Moves focus to the nearest element on the right that matches the given predicate,
   * or None if there is no such element.
   */
  def findNext(p: A => Boolean): Option[Zipper[A]] = findBy((z: Zipper[A]) => z.next)(p)

  /**
   * Moves focus to the previous element on the left that matches the given predicate,
   * or None if there is no such element.
   */
  def findPrevious(p: A => Boolean): Option[Zipper[A]] = findBy((z: Zipper[A]) => z.previous)(p)

  /**
   * A zipper of all positions of the zipper, with focus on the current position.
   */
  def positions: Zipper[Zipper[A]] = {
    val left = unfoldStream[Zipper[A], Zipper[A]](this, (p: Zipper[A]) => p.previous.map(x => (x, x)))
    val right = unfoldStream[Zipper[A], Zipper[A]](this, (p: Zipper[A]) => p.next.map(x => (x, x)))

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
    case (_, Stream.Empty)            =>
      val xs = lefts.reverse
      zipper(rights, xs.head, xs.tail.append(Stream(focus)))
    case (_, _)                       => tryNext
  }

  /**
   * Moves focus to the previous element. If the first element is currently focused, loop to the last element.
   */
  def previousC: Zipper[A] = (lefts, rights) match {
    case (Stream.Empty, Stream.Empty) => this
    case (Stream.Empty, _)            =>
      val xs = rights.reverse
      zipper(xs.tail.append(Stream(focus)), xs.head, lefts)
    case (_, _)                       => tryPrevious
  }

  /**
   * Deletes the focused element and moves focus to the left. If the focus was on the first element,
   * focus is moved to the last element.
   */
  def deleteLeftC: Option[Zipper[A]] = lefts match {
    case l #:: ls     => Some(zipper(ls, l, rights))
    case Stream.Empty => rights match {
      case _ #:: _      => val rrev = rights.reverse; Some(zipper(rrev.tail, rrev.head, Stream.empty))
      case Stream.Empty => None
    }
  }

  /**
   * Deletes the focused element and moves focus to the left. If the focus was on the first element,
   * focus is moved to the last element.
   */
  def deleteLeftCOr[AA >: A](z: => Zipper[AA]): Zipper[AA] =
    deleteLeftC getOrElse z

  /**
   * Deletes the focused element and moves focus to the right. If the focus was on the last element,
   * focus is moved to the first element.
   */
  def deleteRightC: Option[Zipper[A]] = rights match {
    case r #:: rs     => Some(zipper(lefts, r, rs))
    case Stream.Empty => lefts match {
      case _ #:: _      => val lrev = lefts.reverse; Some(zipper(Stream.empty, lrev.head, lrev.tail))
      case Stream.Empty => None
    }
  }

  /**
   * An alias for `deleteRightC`
   */
  def deleteC: Option[Zipper[A]] = deleteRightC

  /**
   * Deletes the focused element and moves focus to the right. If the focus was on the last element,
   * focus is moved to the first element.
   */
  def deleteRightCOr[AA >: A](z: => Zipper[AA]): Zipper[AA] =
    deleteRightC getOrElse z

  def traverse[G[_] : Applicative, B](f: A => G[B]): G[Zipper[B]] = {
    val z = (Zipper.zipper(_: Stream[B], _: B, _: Stream[B])).curried
    val G = Applicative[G]
    import std.stream.streamInstance
    G.apF(G.apF(G.map(Traverse[Stream].traverse[G, A, B](lefts.reverse)(f))(s => z(s.reverse)))(f(focus)))(Traverse[Stream].traverse[G, A, B](rights)(f))
  }

  def ap[B](f: => Zipper[A => B]): Zipper[B] = {
    val ls = lefts.zip(f.lefts) map {
      case (aa, ff) => ff(aa)
    }
    val rs = rights.zip(f.rights) map {
      case (aa, ff) => ff(aa)
    }
    zipper(ls, f.focus(focus), rs)
  }

  override def toString: String = {
    "Zipper(<lefts>, " + focus + ", <rights>)"
  }
}

object Zipper extends ZipperFunctions with ZipperInstances {
  def apply[A](lefts: Stream[A], focus: A, rights: Stream[A]): Zipper[A] =
    zipper(lefts, focus, rights)
}

trait ZipperInstances {
  import Zipper._

  implicit def zipperInstance = new Traverse[Zipper] with Applicative[Zipper] with Comonad[Zipper] {
    override def cojoin[A](a: Zipper[A]): Zipper[Zipper[A]] =
      a.positions
    def cobind[A, B](fa: Zipper[A])(f: Zipper[A] => B): Zipper[B] =
      map(cojoin(fa))(f)
    def copoint[A](p: Zipper[A]): A =
      p.focus
    def traverseImpl[G[_] : Applicative, A, B](za: Zipper[A])(f: A => G[B]): G[Zipper[B]] =
      za traverse f
    override def foldRight[A, B](fa: Zipper[A], z: => B)(f: (A, => B) => B): B =
      fa.foldRight(z)(f)
    override def foldLeft[A, B](fa: Zipper[A], z: B)(f: (B, A) => B): B =
      fa.foldLeft(z)(f)
    def point[A](a: => A): Zipper[A] =
      zipper(Stream.continually(a), a, Stream.continually(a))
    def ap[A, B](fa: => Zipper[A])(f: => Zipper[A => B]): Zipper[B] =
      fa ap f
    override def map[A, B](fa: Zipper[A])(f: A => B): Zipper[B] =
      fa map f
  }

  implicit def zipperEqual[A: Equal]: Equal[Zipper[A]] = new Equal[Zipper[A]] {
    import std.stream.streamEqual
    def equal(a1: Zipper[A], a2: Zipper[A]) =
      streamEqual[A].equal(a1.lefts, a2.lefts) && Equal[A].equal(a1.focus, a2.focus) && streamEqual[A].equal(a1.rights, a2.rights)
  }

  implicit def zipperShow[A: Show]: Show[Zipper[A]] = new Show[Zipper[A]]{
    import std.stream._

    override def show(f: Zipper[A]) =
      Cord("Zipper(",
        Show[Stream[A]].show(f.lefts), ", ",
        Show[A].show(f.focus), ", ",
        Show[Stream[A]].show(f.rights), ")")
  }
}

trait ZipperFunctions {
  def zipper[A](ls: Stream[A], a: A, rs: Stream[A]): Zipper[A] = new Zipper[A] {
    val focus = a
    val lefts = ls
    val rights = rs
  }
}
