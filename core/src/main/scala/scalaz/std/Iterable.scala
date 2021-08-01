package scalaz
package std

import scala.collection.immutable.Seq

trait IterableInstances {

  implicit def iterableShow[CC[X] <: Iterable[X], A: Show]: Show[CC[A]] = Show.show {
    as => list.listShow[A].show(as.toList)
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

  implicit def iterableEqual[CC[X] <: Iterable[X], A: Equal]: Equal[CC[A]] = (a1: CC[A], a2: CC[A]) => {
    val i1 = a1.iterator
    val i2 = a2.iterator
    var b = false

    while (i1.hasNext && i2.hasNext && !b) {
      val x1 = i1.next()
      val x2 = i2.next()

      if (!Equal[A].equal(x1, x2)) {
        b = true
      }
    }

    !(b || i1.hasNext || i2.hasNext)
  }

  implicit def iterableSubtypeFoldable[I[X] <: Iterable[X]]: Foldable[I] =
    new IterableSubtypeFoldable[I] {}
}

private[std] trait IterableSubtypeFoldable[I[X] <: Iterable[X]] extends Foldable[I] {

  override def foldMap[A,B](fa: I[A])(f: A => B)(implicit F: Monoid[B]) = foldLeft(fa, F.zero)((x,y) => Monoid[B].append(x, f(y)))

  override def foldRight[A, B](fa: I[A], b: => B)(f: (A, => B) => B) = fa.foldRight(b)(f(_, _))

  override def foldLeft[A, B](fa: I[A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)

  override def length[A](a: I[A]) = {
    var n = 0
    val i = a.iterator
    while (i.hasNext) {
      n = n + 1
      i.next()
    }
    n
  }

  override final def toList[A](fa: I[A]) = fa.toList
  override final def toVector[A](fa: I[A]) = fa.toVector
  override final def toSet[A](fa: I[A]) = fa.toSet
  override final def toLazyList[A](fa: I[A]) = fa.to(LazyList)

  override final def empty[A](fa: I[A]) = fa.isEmpty

  override final def any[A](fa: I[A])(p: A => Boolean): Boolean =
    fa exists p

  override final def all[A](fa: I[A])(p: A => Boolean): Boolean =
    fa forall p
}

private[std] trait IterableBindRec[F[X] <: Seq[X]] extends BindRec[F] {
  protected[this] def createNewBuilder[A](): scala.collection.mutable.Builder[A, F[A]]

  override final def tailrecM[A, B](a: A)(f: A => F[A \/ B]): F[B] = {
    val bs = createNewBuilder[B]()
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
    bs.result()
  }
}

object iterable extends IterableInstances
