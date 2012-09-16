package scalaz
package std
import scala.collection.GenIterable

trait GenIterableInstances {

  implicit def genIterableShow[CC[X] <: GenIterable[X], A: Show]: Show[CC[A]] = new Show[CC[A]] {
    override def show(as: CC[A]) = "[" +: Cord.mkCord(",", as.map(Show[A].show(_)).toArray:_*) :+ "]"
  }

  /** Lexicographical ordering */
  implicit def genIterableOrder[A](implicit A: Order[A]): Order[GenIterable[A]] = new Order[GenIterable[A]] {
    def order(a1: GenIterable[A], a2: GenIterable[A]): Ordering = {
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

  implicit def genIterableLength: Length[GenIterable] = new Length[GenIterable] {
    def length[A](a: GenIterable[A]) = {
      var n = 0
      val i = a.iterator
      while (i.hasNext) {
        n = n + 1
        i.next
      }
      n
    }
  }

  implicit def genIterableEqual[CC[X] <: GenIterable[X], A: Equal]: Equal[CC[A]] = new Equal[CC[A]] {
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

  implicit def genIterableSubtypeTraverse[I[X] <: GenIterable[X]]: Foldable[I] = new Foldable[I] {
    def foldMap[A,B](fa: I[A])(f: A => B)(implicit F: Monoid[B]) = foldRight(fa, F.zero)((x,y) => Monoid[B].append(f(x), y))

    def foldRight[A, B](fa: I[A], b: => B)(f: (A, => B) => B) = fa.foldRight(b)(f(_, _))

    override def foldLeft[A, B](fa: I[A], b: B)(f: (B, A) => B): B = fa.foldLeft(b)(f)
  }

}

object genIterable extends GenIterableInstances
