package scalaz
package std

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

  implicit def iterableLength: Length[Iterable] = new Length[Iterable] {
    def length[A](a: Iterable[A]) = {
      var n = 0
      val i = a.iterator
      while (i.hasNext) {
        n = n + 1
        i.next
      }
      n
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

  implicit def iterableSubtypeFoldable[I[X] <: Iterable[X]]: Foldable[I] = new Foldable[I] {
    def foldMap[A,B](fa: I[A])(f: A => B)(implicit F: Monoid[B]) = foldRight(fa, F.zero)((x,y) => Monoid[B].append(f(x), y))

    def foldRight[A, B](fa: I[A], b: => B)(f: (A, => B) => B) = fa.foldRight(b)(f(_, _))

    override def foldLeft[A, B](fa: I[A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)
  }
}

object iterable extends IterableInstances
