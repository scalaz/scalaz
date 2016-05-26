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

  private[scalaz] def head: () => A

  private[scalaz] def tail: () => EphemeralStream[A]

  def headOption: Option[A] = {
    if(isEmpty) None
    else Some(head())
  }

  def tailOption: Option[EphemeralStream[A]] = {
    if(isEmpty) None
    else Some(tail())
  }

  def toList: List[A] = {
    def lcons(xs: => List[A])(x: => A) = x :: xs
    foldLeft(Nil: List[A])(lcons _).reverse
  }

  def foldRight[B](z: => B)(f: (=> A) => (=> B) => B): B =
    if (isEmpty) z else f(head())(tail().foldRight(z)(f))

  def foldLeft[B](z: => B)(f: (=> B) => (=> A) => B): B = {
    @annotation.tailrec
    def loop(t: EphemeralStream[A], acc: B): B =
      if (t.isEmpty) acc
      else loop(t.tail(), f(acc)(t.head()))
    loop(this, z)
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

  def tails: EphemeralStream[EphemeralStream[A]] =
    if (isEmpty) EphemeralStream(emptyEphemeralStream)
    else cons(this, tail().tails)

  def inits: EphemeralStream[EphemeralStream[A]] =
    if (isEmpty) EphemeralStream(emptyEphemeralStream)
    else cons(emptyEphemeralStream, tail().inits.map(cons(head(), _)))

  def findM[M[_]: Monad](p: A => M[Boolean]): M[Option[A]] =
    if(isEmpty)
      Monad[M].point(None)
    else {
      val hh = head()
      Monad[M].bind(p(hh))(if (_) Monad[M].point(Some(hh)) else tail() findM p)
    }

  def findMapM[M[_]: Monad, B](f: A => M[Option[B]]): M[Option[B]] = {
    if(isEmpty)
      Monad[M].point(None)
    else{
      val hh = head()
      Monad[M].bind(f(hh)) { case Some(b) => Monad[M].point(Some(b)); case None => tail() findMapM f }
    }
  }

  def reverse: EphemeralStream[A] = {
    def lcons(xs: => List[A])(x: => A) = x :: xs
    apply(foldLeft(Nil: List[A])(lcons _) : _*)
  }

  def zip[B](b: => EphemeralStream[B]): EphemeralStream[(A, B)] =
    if(isEmpty || b.isEmpty)
      emptyEphemeralStream
    else
      cons((head(), b.head()), tail() zip b.tail())

  def unzip[X, Y](implicit ev: A <:< (X, Y)): (EphemeralStream[X], EphemeralStream[Y]) =
    foldRight((emptyEphemeralStream[X], emptyEphemeralStream[Y]))(q => r =>
      (cons(q._1, r._1), cons(q._2, r._2)))

  def alignWith[B, C](f: A \&/ B => C)(b: EphemeralStream[B]): EphemeralStream[C] =
    if(b.isEmpty)
      map(x => f(\&/.This(x)))
    else if(isEmpty)
      b.map(x => f(\&/.That(x)))
    else
      cons(f(\&/.Both(head(), b.head())), tail().alignWith(f)(b.tail()))

  def interleave(q: EphemeralStream[A]): EphemeralStream[A] =
    if(isEmpty)
      q
    else if (q.isEmpty)
      this
    else
      cons(head(), cons(q.head(), tail() interleave q.tail()))

  def take(n: Int): EphemeralStream[A] =
    unfold((n, this)){ case (len, xs) =>
      if(len > 0 && !xs.isEmpty)
        Some((xs.head(), (len - 1, xs.tail())))
      else
        None
    }

  def takeWhile(p: A => Boolean): EphemeralStream[A] =
    if(!isEmpty){
      val h = head()
      if(p(h)) cons(h, tail().takeWhile(p))
      else emptyEphemeralStream
    }else this

  def zipWithIndex: EphemeralStream[(A, Int)] =
    zip(iterate(0)(_ + 1))
}

sealed abstract class EphemeralStreamInstances {
  // TODO more instances
  implicit val ephemeralStreamInstance: MonadPlus[EphemeralStream] with BindRec[EphemeralStream] with Zip[EphemeralStream] with Unzip[EphemeralStream] with Align[EphemeralStream] with Traverse[EphemeralStream] with Cobind[EphemeralStream] with IsEmpty[EphemeralStream] = new MonadPlus[EphemeralStream] with BindRec[EphemeralStream] with Zip[EphemeralStream] with Unzip[EphemeralStream] with Align[EphemeralStream] with Traverse[EphemeralStream] with Cobind[EphemeralStream] with IsEmpty[EphemeralStream] {
    import EphemeralStream._
    override def isEmpty[A](fa: EphemeralStream[A]) = fa.isEmpty
    override def cojoin[A](a: EphemeralStream[A]): EphemeralStream[EphemeralStream[A]] = a match {
      case _ ##:: tl  => if (tl.isEmpty) EphemeralStream(a)
                         else cons(a, cojoin(tl))
      case _ => emptyEphemeralStream
    }
    def cobind[A, B](fa: EphemeralStream[A])(f: EphemeralStream[A] => B): EphemeralStream[B] = map(cojoin(fa))(f)
    def plus[A](a: EphemeralStream[A], b: => EphemeralStream[A]) = a ++ b
    def bind[A, B](fa: EphemeralStream[A])(f: A => EphemeralStream[B]) = fa flatMap f
    def point[A](a: => A) = EphemeralStream(a)
    def empty[A] = EphemeralStream()
    def zip[A, B](a: => EphemeralStream[A], b: => EphemeralStream[B]) = a zip b
    def unzip[A, B](a: EphemeralStream[(A, B)]) = a.unzip
    def alignWith[A, B, C](f: A \&/ B => C) =
      (a, b) =>
        a.alignWith(f)(b)
    override def toEphemeralStream[A](fa: EphemeralStream[A]) = fa
    override def foldRight[A, B](fa: EphemeralStream[A], z: => B)(f: (A, => B) => B): B =
      if(fa.isEmpty) z else f(fa.head(), foldRight(fa.tail(), z)(f))
    override def foldMap[A, B](fa: EphemeralStream[A])(f: A => B)(implicit M: Monoid[B]) =
      this.foldRight(fa, M.zero)((a, b) => M.append(f(a), b))
    override def foldMap1Opt[A, B](fa: EphemeralStream[A])(f: A => B)(implicit B: Semigroup[B]) =
      foldMapRight1Opt(fa)(f)((l, r) => B.append(f(l), r))
    override def foldLeft[A, B](fa: EphemeralStream[A], z: B)(f: (B, A) => B) =
      fa.foldLeft(z)(b => a => f(b, a))

    override def foldMapRight1Opt[A, B](fa: EphemeralStream[A])(z: A => B)(f: (A, => B) => B): Option[B] = {
      def rec(tortoise: EphemeralStream[A], hare: EphemeralStream[A]): B =
        if (hare.isEmpty) z(tortoise.head())
        else f(tortoise.head(), rec(hare, hare.tail()))
      if (fa.isEmpty) None
      else Some(rec(fa, fa.tail()))
    }

    override def foldMapLeft1Opt[A, B](fa: EphemeralStream[A])(z: A => B)(f: (B, A) => B): Option[B] =
      if (fa.isEmpty) None
      else Some(foldLeft(fa.tail(), z(fa.head()))(f))

    override def zipWithL[A, B, C](fa: EphemeralStream[A], fb: EphemeralStream[B])(f: (A, Option[B]) => C) = {
      if(fa.isEmpty) emptyEphemeralStream
      else {
        val (bo, bTail) = if(fb.isEmpty) (None, emptyEphemeralStream[B])
                          else (Some(fb.head()), fb.tail())
        cons(f(fa.head(), bo), zipWithL(fa.tail(), bTail)(f))
      }
    }
    override def zipWithR[A, B, C](fa: EphemeralStream[A], fb: EphemeralStream[B])(f: (Option[A], B) => C) =
      zipWithL(fb, fa)((b, a) => f(a, b))
    def traverseImpl[G[_], A, B](fa: EphemeralStream[A])(f: A => G[B])(implicit G: Applicative[G]): G[EphemeralStream[B]] = {
      val seed: G[EphemeralStream[B]] = G.point(EphemeralStream[B]())

      fa.foldRight(seed) {
        x => ys => G.apply2(f(x), ys)((b, bs) => EphemeralStream.cons(b, bs))
      }
    }
    override def index[A](fa: EphemeralStream[A], i: Int): Option[A] = {
      if(i < 0)
        None
      else{
        var n = i
        var these = fa
        while (!these.isEmpty && n > 0){
          n -= 1
          these = these.tail()
        }
        if (these.isEmpty) None else Some(these.head())
      }
    }
    def tailrecM[A, B](f: A => EphemeralStream[A \/ B])(a: A): EphemeralStream[B] = {
      def go(s: EphemeralStream[A \/ B]): EphemeralStream[B] = {
        @annotation.tailrec
        def rec(abs: EphemeralStream[A \/ B]): EphemeralStream[B] =
          abs match {
            case \/-(b) ##:: tail => cons(b, go(tail))
            case -\/(a0) ##:: tail => rec(f(a0) ++ tail)
            case _ => emptyEphemeralStream
          }
        rec(s)
      }
      go(f(a))
    }
  }

  import std.list._

  implicit def ephemeralStreamEqual[A: Equal]: Equal[EphemeralStream[A]] = Equal[List[A]] contramap {(_: EphemeralStream[A]).toList}
}

object EphemeralStream extends EphemeralStreamInstances {

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
      Some((a, fa))
    }

  def range(lower: Int, upper: Int): EphemeralStream[Int] =
    if (lower >= upper) emptyEphemeralStream else cons(lower, range(lower + 1, upper))

  def fromStream[A](s: => Stream[A]): EphemeralStream[A] = s match {
    case Stream() => emptyEphemeralStream
    case h #:: t  => cons(h, fromStream(t))
  }

  def toIterable[A](e: EphemeralStream[A]): Iterable[A] = new Iterable[A] {
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

  def apply[A]: EphemeralStream[A] =
    emptyEphemeralStream

  def apply[A](as: A*): EphemeralStream[A] = {
    val as0 = as match{
      case indexedSeq: collection.IndexedSeq[A] => indexedSeq
      case other => other.toIndexedSeq
    }
    val size = as.size
    unfold(0)(b =>
      if (b < size) Some((as0(b), b + 1))
      else None)
  }

  class ConsWrap[A](e: => EphemeralStream[A]) {
    def ##::(h: A): EphemeralStream[A] = cons(h, e)
  }

  implicit def consWrapper[A](e: => EphemeralStream[A]): ConsWrap[A] =
    new ConsWrap[A](e)

  object ##:: {
    def unapply[A](xs: EphemeralStream[A]): Option[(A, EphemeralStream[A])] =
      if (xs.isEmpty) None
      else Some((xs.head(), xs.tail()))
  }
}
