package scalaz
package std

import collection.SeqLike
import collection.generic.{CanBuildFrom, SeqFactory, GenericTraversableTemplate}
import collection.immutable.Seq

trait IterableInstances {

  implicit def iterableShow[CC[X] <: Iterable[X], A: Show]: Show[CC[A]] = new Show[CC[A]] {
    override def show(as: CC[A]) = "[" +: Cord.mkCord(",", as.map(Show[A].show(_)).toSeq:_*) :+ "]"
  }

  /** Lexicographical ordering */
  implicit def iterableOrder[A](implicit A: Order[A]): Order[Iterable[A]] = new Order[Iterable[A]] {
    def order(a1: Iterable[A], a2: Iterable[A]): Ordering = {
      import scalaz.Ordering._
      val i1 = a1.iterator
      val i2 = a2.iterator

      while (i1.hasNext && i2.hasNext) {
        val a1 = i1.next()
        val a2 = i2.next()

        val o = A.order(a1, a2)
        if (o != EQ) {
          return o
        }
      }
      anyVal.booleanInstance.order(i1.hasNext, i2.hasNext)
    }
  }

  implicit def iterableEqual[CC[X] <: Iterable[X], A: Equal]: Equal[CC[A]] = new Equal[CC[A]] {
    def equal(a1: CC[A], a2: CC[A]) = {
      val i1 = a1.iterator
      val i2 = a2.iterator
      var b = false

      while (i1.hasNext && i2.hasNext && !b) {
        val x1 = i1.next
        val x2 = i2.next

        if (!Equal[A].equal(x1, x2)) {
          b = true
        }
      }

      !(b || i1.hasNext || i2.hasNext)
    }
  }

  implicit def iterableSubtypeFoldable[I[X] <: Iterable[X]]: Foldable[I] =
    new IterableSubtypeFoldable[I] {}
}

private[std] trait IterableSubtypeFoldable[I[X] <: Iterable[X]] extends Foldable[I] {
  import collection.generic.CanBuildFrom

  override def foldMap[A,B](fa: I[A])(f: A => B)(implicit F: Monoid[B]) = foldLeft(fa, F.zero)((x,y) => Monoid[B].append(x, f(y)))

  override def foldRight[A, B](fa: I[A], b: => B)(f: (A, => B) => B) = fa.foldRight(b)(f(_, _))

  override def foldLeft[A, B](fa: I[A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)

  override def length[A](a: I[A]) = {
    var n = 0
    val i = a.iterator
    while (i.hasNext) {
      n = n + 1
      i.next
    }
    n
  }

  override final def toList[A](fa: I[A]) = fa.toList
  override final def toVector[A](fa: I[A]) = fa.toVector
  override final def toSet[A](fa: I[A]) = fa.toSet
  override final def toStream[A](fa: I[A]) = fa.toStream

  override final def to[A, G[_]](fa: I[A])(implicit c: CanBuildFrom[Nothing, A, G[A]]): G[A] =
    fa.to[G]

  override final def empty[A](fa: I[A]) = fa.isEmpty

  override final def any[A](fa: I[A])(p: A => Boolean): Boolean =
    fa exists p

  override final def all[A](fa: I[A])(p: A => Boolean): Boolean =
    fa forall p
}

private[std] trait StrictOrLazySeqSubtypeCovariant[F[+X] <: Seq[X] with GenericTraversableTemplate[X, F] with SeqLike[X, F[X]]]
    extends Traverse[F] with MonadPlus[F] with BindRec[F] with Zip[F] with Unzip[F] with IsEmpty[F] {
  import Liskov.<~<

  protected[this] val Factory: SeqFactory[F]

  protected[this] implicit def canBuildFrom[A]: CanBuildFrom[F[_], A, F[A]]

  override final def length[A](fa: F[A]) = fa.length
  override final def point[A](a: => A) = Factory(a)
  override final def bind[A, B](fa: F[A])(f: A => F[B]) = fa flatMap f
  override final def empty[A] = Factory.empty[A]
  override final def isEmpty[A](fa: F[A]) = fa.isEmpty
  override final def map[A, B](l: F[A])(f: A => B) = l map f
  override final def widen[A, B](l: F[A])(implicit ev: A <~< B) = Liskov.co(ev)(l)
  override final def filter[A](fa: F[A])(p: A => Boolean): F[A] = fa filter p

  override final def zip[A, B](a: => F[A], b: => F[B]) = {
    val _a = a
    if(_a.isEmpty) empty
    else _a zip b
  }
  override final def unzip[A, B](a: F[(A, B)]) = a.unzip

  override final def traverseS[S,A,B](l: F[A])(f: A => State[S,B]): State[S, F[B]] = {
    State{s: S =>
      val buf = canBuildFrom[B].apply(l)
      var cur = s
      l.foreach { a => val bs = f(a)(cur); buf += bs._2; cur = bs._1 }
      (cur, buf.result)
    }
  }
}

private[std] trait StrictSeqSubtypeCovariant[F[+X] <: Seq[X] with GenericTraversableTemplate[X, F] with SeqLike[X, F[X]]]
    extends StrictOrLazySeqSubtypeCovariant[F] {
  override final def plus[A](a: F[A], b: => F[A]) = a ++ b
  override final def index[A](fa: F[A], i: Int) = fa.lift.apply(i)

  override final def tailrecM[A, B](a: A)(f: A => F[A \/ B]): F[B] = {
    val bs = canBuildFrom[B].apply()
    @scala.annotation.tailrec
    def go(xs: List[Seq[A \/ B]]): Unit =
      xs match {
        case (\/-(b) +: tail) :: rest =>
          bs += b
          go(tail :: rest)
        case (-\/(a0) +: tail) :: rest =>
          go(f(a0) :: tail :: rest)
        case _ :: rest =>
          go(rest)
        case Nil =>
      }
    go(List(f(a)))
    bs.result
  }
}

object iterable extends IterableInstances
