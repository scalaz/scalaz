package scalaz

import annotation.tailrec
import Maybe.{Empty, Just, just}

/**
 * Provides a pointed stream, which is a non-empty zipper-like stream structure that tracks an index (focus)
 * position in a stream. Focus can be moved forward and backwards through the stream, elements can be inserted
 * before or after the focused position, and the focused item can be deleted.
 * <p/>
 * Based on the pointedlist library by Jeff Wheeler.
 */
final case class Zipper[A](lefts: LazyList[A], focus: A, rights: LazyList[A]) {
  import Zipper._

  def map[B](f: A => B): Zipper[B] =
    zipper(lefts map f, f(focus), rights map f)

  /**
   * Get the Stream representation of this Zipper. This fully traverses `lefts`. `rights` is
   * not evaluated.
   */
  def toStream: Stream[A] =
    (lefts.reverse ++ focus #:: rights).toStream

  /**
   * Get the LazyList representation of this Zipper. This fully traverses `lefts`. `rights` is
   * not evaluated.
   */
  def toLazyList: LazyList[A] =
    lefts.reverse ++ focus #:: rights

  /**
   * Update the focus in this zipper.
   */
  def update(focus: A): Zipper[A] = {
    this.copy(this.lefts, focus, this.rights)
  }

  /**
   * Apply f to the focus and update with the result.
   */
  def modify(f: A => A): Zipper[A] = this.update(f(this.focus))

  /**
   * Possibly moves to next element to the right of focus.
   */
  def next: Maybe[Zipper[A]] = rights match {
    case LazyList() => Maybe.empty
    case r #:: rs     => just(zipper(LazyList.cons(focus, lefts), r, rs))
  }

  /**
   * Possibly moves to next element to the right of focus.
   */
  def nextOr(z: => Zipper[A]): Zipper[A] =
    next getOrElse z

  /**
   * Possibly moves to the previous element to the left of focus.
   */
  def previous: Maybe[Zipper[A]] = lefts match {
    case LazyList() => Maybe.empty
    case l #:: ls     => just(zipper(ls, l, LazyList.cons(focus, rights)))
  }

  /**
   * Possibly moves to previous element to the left of focus.
   */
  def previousOr(z: => Zipper[A]): Zipper[A] =
    previous getOrElse z

  /**
   * Moves to the previous element to the left of focus, or error if there is no element on the left.
   */
  def tryPrevious: Zipper[A] = previousOr(sys.error("cannot move to previous element"))

  /**
   * An alias for insertRight
   */
  def insert(a: A): Zipper[A] = insertRight(a)

  /**
   * Inserts an element to the left of focus and focuses on the new element.
   */
  def insertLeft(y: A): Zipper[A] = zipper(lefts, y, focus #:: rights)

  /**
   * Inserts an element to the right of focus and focuses on the new element.
   */
  def insertRight(y: A): Zipper[A] = zipper(focus #:: lefts, y, rights)

  /**
   * An alias for `deleteRight`
   */
  def delete: Maybe[Zipper[A]] = deleteRight

  /**
   * Deletes the element at focus and moves the focus to the left. If there is no element on the left,
   * focus is moved to the right.
   */
  def deleteLeft: Maybe[Zipper[A]] = lefts match {
    case l #:: ls     => just(zipper(ls, l, rights))
    case LazyList() => rights match {
      case r #:: rs     => just(zipper(LazyList.empty, r, rs))
      case LazyList() => Maybe.empty
    }
  }

  /**
   * Deletes the element at focus and moves the focus to the left. If there is no element on the left,
   * focus is moved to the right.
   */
  def deleteLeftOr(z: => Zipper[A]): Zipper[A] =
    deleteLeft getOrElse z

  /**
   * Deletes the element at focus and moves the focus to the right. If there is no element on the right,
   * focus is moved to the left.
   */
  def deleteRight: Maybe[Zipper[A]] = rights match {
    case r #:: rs     => just(zipper(lefts, r, rs))
    case LazyList() => lefts match {
      case l #:: ls     => just(zipper(ls, l, LazyList.empty))
      case LazyList() => Maybe.empty
    }
  }

  /**
   * Deletes the element at focus and moves the focus to the right. If there is no element on the right,
   * focus is moved to the left.
   */
  def deleteRightOr(z: => Zipper[A]): Zipper[A] =
    deleteRight getOrElse z

  /**
   * Deletes all elements except the focused element.
   */
  def deleteOthers: Zipper[A] = zipper(LazyList(), focus, LazyList())

  def foldLeft[B](b: B)(f: (B, A) => B): B =
    LazyList.cons(focus, rights).foldLeft(lefts.foldRight(b)((a, b) => f(b, a)))(f)

  def foldRight[B](b: => B)(f: (A, => B) => B): B =
    lefts.foldLeft(LazyList.cons(focus, rights).foldRight(b)((a, b) => f(a, b)))((a, b) => f(b, a))

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
  def withFocus: Zipper[(A, Boolean)] = zipper(lefts.zip(LazyList.continually(false)), (focus, true), rights.zip(LazyList.continually(false)))

  /**
   * Moves focus n elements in the zipper, or Maybe.empty if there is no such element.
   *
   * @param  n  number of elements to move (positive is forward, negative is backwards)
   */
  def move(n: Int): Maybe[Zipper[A]] = {
    @tailrec
    def move0(z: Maybe[Zipper[A]], n: Int): Maybe[Zipper[A]] =
      if (n > 0 && rights.isEmpty || n < 0 && lefts.isEmpty) Maybe.empty
      else {
        if (n == 0) z
        else if (n > 0) move0(z flatMap ((_: Zipper[A]).next), n - 1)
        else move0(z flatMap ((_: Zipper[A]).previous), n + 1)
      }
    move0(just(this), n)
  }

  /**
   * Moves focus to the start of the zipper.
   */
  def start: Zipper[A] = {
    val rights = this.lefts.reverse ++ focus #:: this.rights
    this.copy(LazyList(), rights.head, rights.tail)
  }

  /**
   * Moves focus to the end of the zipper.
   */
  def end: Zipper[A] = {
    val lefts = this.rights.reverse ++ focus #:: this.lefts
    this.copy(lefts.tail, lefts.head, LazyList.empty)
  }

  /**
   * Moves focus to the nth element of the zipper, or the default if there is no such element.
   */
  def moveOr(n: Int, z: => Zipper[A]): Zipper[A] =
    move(n) getOrElse z

  /**
   * Moves focus to the nearest element matching the given predicate, preferring the left,
   * or Maybe.empty if no element matches.
   */
  def findZ(p: A => Boolean): Maybe[Zipper[A]] =
    if (p(focus)) just(this)
    else {
      val c = this.positions
      Maybe.fromOption(std.lazylist.interleave(c.lefts, c.rights).find(x => p(x.focus)))
    }

  /**
   * Moves focus to the nearest element matching the given predicate, preferring the left,
   * or the default if no element matches.
   */
  def findZor(p: A => Boolean, z: => Zipper[A]): Zipper[A] =
    findZ(p) getOrElse z

  /**
   * Given a traversal function, find the first element along the traversal that matches a given predicate.
   */
  def findBy(f: Zipper[A] => Maybe[Zipper[A]])(p: A => Boolean): Maybe[Zipper[A]] = {
    @tailrec
    def go(zopt: Maybe[Zipper[A]]): Maybe[Zipper[A]] = {
      zopt match {
        case Just(z) => if (p(z.focus)) just(z) else go(f(z))
        case Empty()    => Maybe.empty
      }
    }
    go(f(this))
  }

  /**
   * Moves focus to the nearest element on the right that matches the given predicate,
   * or Maybe.empty if there is no such element.
   */
  def findNext(p: A => Boolean): Maybe[Zipper[A]] = findBy((z: Zipper[A]) => z.next)(p)

  /**
   * Moves focus to the previous element on the left that matches the given predicate,
   * or Maybe.empty if there is no such element.
   */
  def findPrevious(p: A => Boolean): Maybe[Zipper[A]] = findBy((z: Zipper[A]) => z.previous)(p)

  /**
   * A zipper of all positions of the zipper, with focus on the current position.
   */
  def positions: Zipper[Zipper[A]] = {
    val left = std.lazylist.unfoldm(this)(_.previous.map(x => (x, x)))
    val right = std.lazylist.unfoldm(this)(_.next.map(x => (x, x)))

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
    case (LazyList(), LazyList()) => this
    case (_, LazyList())            =>
      val xs = lefts.reverse
      zipper(rights, xs.head, xs.tail ++ LazyList(focus))
    case (_, r #:: rs)                =>
      zipper(LazyList.cons(focus, lefts), r, rs)
  }

  /**
   * Moves focus to the previous element. If the first element is currently focused, loop to the last element.
   */
  def previousC: Zipper[A] = (lefts, rights) match {
    case (LazyList(), LazyList()) => this
    case (LazyList(), _)            =>
      val xs = rights.reverse
      zipper(xs.tail ++ LazyList(focus), xs.head, lefts)
    case (_, _)                       => tryPrevious
  }

  /**
   * Deletes the focused element and moves focus to the left. If the focus was on the first element,
   * focus is moved to the last element.
   */
  def deleteLeftC: Maybe[Zipper[A]] = lefts match {
    case l #:: ls     => just(zipper(ls, l, rights))
    case LazyList() => rights match {
      case _ #:: _      => val rrev = rights.reverse; just(zipper(rrev.tail, rrev.head, LazyList.empty))
      case LazyList() => Maybe.empty
    }
  }

  /**
   * Deletes the focused element and moves focus to the left. If the focus was on the first element,
   * focus is moved to the last element.
   */
  def deleteLeftCOr(z: => Zipper[A]): Zipper[A] =
    deleteLeftC getOrElse z

  /**
   * Deletes the focused element and moves focus to the right. If the focus was on the last element,
   * focus is moved to the first element.
   */
  def deleteRightC: Maybe[Zipper[A]] = rights match {
    case r #:: rs     => just(zipper(lefts, r, rs))
    case LazyList() => lefts match {
      case _ #:: _      => val lrev = lefts.reverse; just(zipper(LazyList.empty, lrev.head, lrev.tail))
      case LazyList() => Maybe.empty
    }
  }

  /**
   * An alias for `deleteRightC`
   */
  def deleteC: Maybe[Zipper[A]] = deleteRightC

  /**
   * Deletes the focused element and moves focus to the right. If the focus was on the last element,
   * focus is moved to the first element.
   */
  def deleteRightCOr(z: => Zipper[A]): Zipper[A] =
    deleteRightC getOrElse z

  def traverse[G[_] : Applicative, B](f: A => G[B]): G[Zipper[B]] = {
    val z = (Zipper.zipper(_: LazyList[B], _: B, _: LazyList[B])).curried
    val G = Applicative[G]
    import std.lazylist.lazylistInstance
    G.apF(G.apF(G.map(Traverse[LazyList].traverse[G, A, B](lefts.reverse)(f))(s => z(s.reverse)))(f(focus)))(Traverse[LazyList].traverse[G, A, B](rights)(f))
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

object Zipper extends ZipperInstances {
  def zipper[A](ls: LazyList[A], a: A, rs: LazyList[A]): Zipper[A] =
    Zipper(ls, a, rs)
}

sealed abstract class ZipperInstances {
  import Zipper._

  implicit val zipperInstance: Traverse1[Zipper] & Applicative[Zipper] & Comonad[Zipper] = new Traverse1[Zipper] with Applicative[Zipper] with Comonad[Zipper] {
    import std.lazylist._
    override def cojoin[A](a: Zipper[A]): Zipper[Zipper[A]] =
      a.positions
    def cobind[A, B](fa: Zipper[A])(f: Zipper[A] => B): Zipper[B] =
      map(cojoin(fa))(f)
    def copoint[A](p: Zipper[A]): A =
      p.focus
    override def traverseImpl[G[_] : Applicative, A, B](za: Zipper[A])(f: A => G[B]): G[Zipper[B]] =
      za traverse f
    override def foldRight[A, B](fa: Zipper[A], z: => B)(f: (A, => B) => B): B =
      fa.foldRight(z)(f)
    override def foldLeft[A, B](fa: Zipper[A], z: B)(f: (B, A) => B): B =
      fa.foldLeft(z)(f)
    override def foldMap[A, B](fa: Zipper[A])(f: A => B)(implicit F: Monoid[B]) =
      fa.foldLeft(F.zero)((b, a) => F.append(b, f(a)))
    def point[A](a: => A): Zipper[A] =
      zipper(LazyList.continually(a), a, LazyList.continually(a))
    def ap[A, B](fa: => Zipper[A])(f: => Zipper[A => B]): Zipper[B] =
      fa ap f
    override def map[A, B](fa: Zipper[A])(f: A => B): Zipper[B] =
      fa map f
    override def all[A](fa: Zipper[A])(f: A => Boolean) =
      fa.lefts.forall(f) && f(fa.focus) && fa.rights.forall(f)
    override def any[A](fa: Zipper[A])(f: A => Boolean) =
      fa.lefts.exists(f) || f(fa.focus) || fa.rights.exists(f)
    override def foldMap1[A, B](fa: Zipper[A])(f: A => B)(implicit F: Semigroup[B]) =
      fa.rights.foldLeft(
        Foldable[LazyList].foldMapRight1Opt(fa.lefts)(f)((a, b) => F.append(b, f(a))) match {
          case Some(b) => F.append(b, f(fa.focus))
          case None => f(fa.focus)
        }
      )((b, a) => F.append(b, f(a)))
    override def foldMapRight1[A, B](fa: Zipper[A])(z: A => B)(f: (A, => B) => B) =
      Foldable[LazyList].foldLeft(
        fa.lefts,
        Foldable[LazyList].foldMapRight1Opt(fa.rights)(z)(f) match {
          case Some(b) => f(fa.focus, b)
          case None => z(fa.focus)
        }
      )((b, a) => f(a, b))
    override def foldMapLeft1[A, B](fa: Zipper[A])(z: A => B)(f: (B, A) => B) =
      fa.rights.foldLeft(
        Foldable[LazyList].foldMapRight1Opt(fa.lefts)(z)((a, b) => f(b, a)) match {
          case Some(b) => f(b, fa.focus)
          case None => z(fa.focus)
        }
      )(f)
    override def traverse1Impl[G[_], A, B](fa: Zipper[A])(f: A => G[B])(implicit G: Apply[G]) = {
      val F = Traverse1[OneAnd[LazyList, *]]
      fa.lefts.reverse match {
        case h1 #:: t1 =>
          val x = G.map(F.traverse1(OneAnd(h1, t1))(f)) { s => (s.head #:: s.tail).reverse }
          fa.rights match {
            case h2 #:: t2 =>
              G.apply3(x, f(fa.focus), F.traverse1(OneAnd(h2, t2))(f)) { (l, z, r) =>
                Zipper(l, z, r.head #:: r.tail)
              }
            case LazyList() =>
              G.apply2(x, f(fa.focus)) { (l, z) =>
                Zipper(l, z, LazyList())
              }
          }
        case LazyList() =>
          fa.rights match {
            case h2 #:: t2 =>
              G.apply2(f(fa.focus), F.traverse1(OneAnd(h2, t2))(f)) { (z, r) =>
                Zipper(LazyList(), z, r.head #:: r.tail)
              }
            case LazyList() =>
              G.map(f(fa.focus)) { z =>
                Zipper(LazyList(), z, LazyList())
              }
          }
      }
    }
  }

  implicit def zipperEqual[A: Equal]: Equal[Zipper[A]] = new Equal[Zipper[A]] {
    import std.lazylist.lazylistEqual
    private[this] val A = Equal[LazyList[A]]
    def equal(a1: Zipper[A], a2: Zipper[A]) =
      A.equal(a1.lefts, a2.lefts) && Equal[A].equal(a1.focus, a2.focus) && A.equal(a1.rights, a2.rights)
  }

  implicit def zipperShow[A: Show]: Show[Zipper[A]] = Show.show { f =>
    import std.lazylist._
    import syntax.show._
    val left = Show[LazyList[A]].show(f.lefts)
    val right = Show[LazyList[A]].show(f.rights)
    cord"Zipper($left,${f.focus},$right)"
  }

  implicit val covariant: IsCovariant[Zipper] = IsCovariant.force
}
