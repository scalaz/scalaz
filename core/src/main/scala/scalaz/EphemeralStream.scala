package scalaz

import java.lang.ref.WeakReference

sealed trait EphemeralStream[A] {

  import EphemeralStream._

  def isEmpty: Boolean

  def head: () => A

  def tail: () => EphemeralStream[A]

  def toList: List[A] = {
    def lcons(xs: => List[A])(x: => A) = x :: xs
    foldLeft(Nil: List[A])(lcons _).reverse
  }

  def foldRight[B](z: => B)(f: (=> A) => (=> B) => B): B =
    if (isEmpty) z else f(head())(tail().foldRight(z)(f))

  def foldLeft[B](z: => B)(f: (=> B) => (=> A) => B): B = {
    var t = this
    var acc = z
    while (!t.isEmpty) {
      acc = f(acc)(t.head())
      t = t.tail()
    }
    acc
  }

  def filter(p: A => Boolean): EphemeralStream[A] = {
    var rest = this dropWhile (!p(_))
    if (rest.isEmpty) emptyEphemeralStream
    else cons(rest.head(), rest.tail() filter p)
  }

  def dropWhile(p: A => Boolean): EphemeralStream[A] = {
    var these: EphemeralStream[A] = this
    while (!these.isEmpty && p(these.head())) these = these.tail()
    these
  }

  def ++(e: => EphemeralStream[A]): EphemeralStream[A] =
    foldRight[EphemeralStream[A]](e)((cons[A](_, _)).curried)

  def flatMap[B](f: A => EphemeralStream[B]): EphemeralStream[B] =
    foldRight[EphemeralStream[B]](emptyEphemeralStream)(h => t => f(h) ++ t)

  def map[B](f: A => B): EphemeralStream[B] =
    flatMap(x => EphemeralStream(f(x)))

  def length = {
    def addOne(c: => Int)(a: => A) = 1 + c
    foldLeft(0)(addOne _)
  }
}

object EphemeralStream extends EphemeralStreams {
  def apply[A]: EphemeralStream[A] =
    emptyEphemeralStream
}

trait EphemeralStreams {
  def emptyEphemeralStream[A]: EphemeralStream[A] = new EphemeralStream[A] {
    def isEmpty = true

    def head: () => Nothing = () => sys.error("head of empty stream")

    def tail: () => Nothing = () => sys.error("tail of empty stream")
  }

  def cons[A](a: => A, as: => EphemeralStream[A]) = new EphemeralStream[A] {
    def isEmpty = false

    val head = weakMemo(a)
    val tail = weakMemo(as)
  }

  def apply[A](as: A*): EphemeralStream[A] =
    unfold(0, (b: Int) =>
      if (b < as.size) Some((as(b), b + 1))
      else None)

  implicit val EphemeralStreamPointed: Pointed[EphemeralStream] = new Pointed[EphemeralStream] {
    def point[A](a: => A) = EphemeralStream(a)
  }

  implicit val EphemeralStreamBind: Bind[EphemeralStream] = new Bind[EphemeralStream] {
    def bind[A, B](f: A => EphemeralStream[B]) =
      _ flatMap f
  }

  def unfold[A, B](b: => B, f: B => Option[(A, B)]): EphemeralStream[A] =
    f(b) match {
      case None => emptyEphemeralStream
      case Some((a, r)) => cons(a, unfold(r, f))
    }

  def iterate[A](start: A)(f: A => A): EphemeralStream[A] =
    unfold(start, (a: A) => {
      val fa = f(a)
      Some((fa, fa))
    })

  def range(lower: Int, upper: Int): EphemeralStream[Int] =
    if (lower >= upper) emptyEphemeralStream else cons(lower, range(lower + 1, upper))

  def fromStream[A](s: => Stream[A]): EphemeralStream[A] = s match {
    case Stream() => emptyEphemeralStream
    case h #:: t => cons(h, fromStream(t))
  }

  implicit def toIterable[A](e: EphemeralStream[A]): Iterable[A] = new Iterable[A] {
    def iterator = new Iterator[A] {
      var cur = e

      def next = {
        val t = cur.head()
        cur = cur.tail()
        t
      }

      def hasNext = !cur.isEmpty
    }
  }

  def weakMemo[V](f: => V): () => V = {
    val latch = new Object
    @volatile var v: Option[WeakReference[V]] = None
    () => {
      val a = v.map(x => x.get)
      if (a.isDefined && a.get != null) a.get
      else latch.synchronized {
        val x = f
        v = Some(new WeakReference(x))
        x
      }
    }
  }

}
