package scalaz

import java.lang.ref.WeakReference

/** Like [[scala.collection.immutable.Stream]], but doesn't save
  * computed values.  As such, it can be used to represent similar
  * things, but without the space leak problem frequently encountered
  * using that type.
  */
sealed abstract class EphemeralStream[A] {

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
    val rest = this dropWhile (!p(_))
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

  def findM[M[_]: Monad](p: A => M[Boolean]): M[Option[A]] =
    if(isEmpty)
      Monad[M].point(None)
    else {
      val hh = head()
      Monad[M].bind(p(hh))(if (_) Monad[M].point(Some(hh)) else tail() findM p)
    }

  def reverse: EphemeralStream[A] =
    foldLeft(EphemeralStream.emptyEphemeralStream[A])(a => b => EphemeralStream.cons(b, a))

  def zip[B](b: => EphemeralStream[B]): EphemeralStream[(A, B)] =
    if(isEmpty && b.isEmpty)
      emptyEphemeralStream
    else
      cons((head(), b.head()), tail() zip b.tail())

  def unzip[X, Y](implicit ev: A =:= (X, Y)): (EphemeralStream[X], EphemeralStream[Y]) =
    foldRight((emptyEphemeralStream[X], emptyEphemeralStream[Y]))(q => r =>
      (cons(q._1, r._1), cons(q._2, r._2)))

  def interleave(q: EphemeralStream[A]): EphemeralStream[A] =
    if(isEmpty)
      q
    else if (q.isEmpty)
      this
    else
      cons(head(), cons(q.head(), tail() interleave q.tail()))

}

object EphemeralStream extends EphemeralStreamFunctions with EphemeralStreamInstances {
  def apply[A]: EphemeralStream[A] =
    emptyEphemeralStream

  def apply[A](as: A*): EphemeralStream[A] =
    unfold(0)(b =>
      if (b < as.size) Some((as(b), b + 1))
      else None)
}

trait EphemeralStreamInstances {
  // TODO more instances
  implicit val ephemeralStreamInstance = new MonadPlus[EphemeralStream] with Zip[EphemeralStream] with Unzip[EphemeralStream] with Traverse[EphemeralStream] {
    def plus[A](a: EphemeralStream[A], b: => EphemeralStream[A]) = a ++ b
    def bind[A, B](fa: EphemeralStream[A])(f: A => EphemeralStream[B]) = fa flatMap f
    def point[A](a: => A) = EphemeralStream(a)
    def empty[A] = EphemeralStream()
    def zip[A, B](a: => EphemeralStream[A], b: => EphemeralStream[B]) = a zip b
    def unzip[A, B](a: EphemeralStream[(A, B)]) = a.unzip

    def traverseImpl[G[_], A, B](fa: EphemeralStream[A])(f: A => G[B])(implicit G: Applicative[G]): G[EphemeralStream[B]] = {
      val seed: G[EphemeralStream[B]] = G.point(EphemeralStream[B]())

      fa.foldRight(seed) {
        x => ys => G.apply2(f(x), ys)((b, bs) => EphemeralStream.cons(b, bs))
      }
    }

  }

  import std.list._

  implicit def ephemeralStreamEqual[A: Equal]: Equal[EphemeralStream[A]] = Equal[List[A]] contramap {(_: EphemeralStream[A]).toList}
}

trait EphemeralStreamFunctions {
  type EStream[A] = EphemeralStream[A]

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

  def unfold[A, B](b: => B)(f: B => Option[(A, B)]): EphemeralStream[A] =
    f(b) match {
      case None         => emptyEphemeralStream
      case Some((a, r)) => cons(a, unfold(r)(f))
    }

  def iterate[A](start: A)(f: A => A): EphemeralStream[A] =
    unfold(start){ a =>
      val fa = f(a)
      Some((fa, fa))
    }

  def range(lower: Int, upper: Int): EphemeralStream[Int] =
    if (lower >= upper) emptyEphemeralStream else cons(lower, range(lower + 1, upper))

  def fromStream[A](s: => Stream[A]): EphemeralStream[A] = s match {
    case Stream() => emptyEphemeralStream
    case h #:: t  => cons(h, fromStream(t))
  }

  implicit def toIterable[A](e: EphemeralStream[A]): Iterable[A] = new Iterable[A] {
    def iterator = new Iterator[A] {
      var cur = e

      def next() = {
        val t = cur.head()
        cur = cur.tail()
        t
      }

      def hasNext = !cur.isEmpty
    }
  }

  def weakMemo[V](f: => V): () => V = {
    val latch = new Object
    // TODO I don't think this annotation does anything, as `v` isn't a class member.
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
