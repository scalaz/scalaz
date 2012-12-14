package scalaz
package std

trait SetInstances {
  implicit val setInstance = new Traverse[Set] with MonadPlus[Set] with Each[Set] with Length[Set] with IsEmpty[Set] {
    def each[A](fa: Set[A])(f: (A) => Unit) = fa foreach f
    def length[A](fa: Set[A]) = fa.size
    def point[A](a: => A) = Set(a)
    def bind[A, B](fa: Set[A])(f: A => Set[B]) = fa flatMap f
    def empty[A] = Set()
    def plus[A](a: Set[A], b: => Set[A]) = a ++ b
    def isEmpty[A](fa: Set[A]) = fa.isEmpty
    override def map[A, B](l: Set[A])(f: A => B) = l map f

    // TODO duplication with ListInstances
    def traverseImpl[F[_], A, B](l: Set[A])(f: A => F[B])(implicit F: Applicative[F]) = {
      DList.fromList(l.toList).foldr(F.point(Set[B]())) {
        (a, fbs) => F.apply2(f(a), fbs)((a, b) => b + a)
      }
    }

    override def foldRight[A, B](fa: Set[A], z: => B)(f: (A, => B) => B) = {
      import scala.collection.mutable.ArrayStack
      val s = new ArrayStack[A]
      fa.foreach(a => s += a)
      var r = z
      while (!s.isEmpty) {
        r = f(s.pop, r)
      }
      r
    }
  }

  import Ordering._

  /**
   * We could derive set equality from `Equal[A]`, but it would be `O(n^2)`.
   * Instead, we require `Order[A]`, reducing the complexity to `O(log n)`
   *
   * If `Equal[A].equalIsNatural == true`, than `Any#==` is used.
   */
  implicit def setOrder[A: Order]: Order[Set[A]] = new Order[Set[A]] {
    def order(a1: Set[A], a2: Set[A]) = {
      import anyVal._
      import scala.math.Ordering.Implicits._
      implicit val o = Order[A].toScalaOrdering
      implicit val so = Order.fromScalaOrdering(seqDerivedOrdering[Seq, A])
      Order[Int].order(a1.size, a2.size) match {
        case EQ => Order.orderBy((s: Set[A]) => s.toSeq.sorted).order(a1, a2)
        case x => x
      }
    }

    override def equal(a1: Set[A], a2: Set[A]) = {
      if (equalIsNatural) a1 == a2
      else {
        implicit val x = Order[A].toScalaOrdering
        import scala.collection.immutable.TreeSet
        val s1 = TreeSet[A](a1.toSeq: _*)
        val s2 = TreeSet[A](a2.toSeq: _*)
        s1.toStream.corresponds(s2.toStream)(Order[A].equal)
      }
    }
    override val equalIsNatural: Boolean = Equal[A].equalIsNatural
  }

  implicit def setMonoid[A]: Monoid[Set[A]] = new Monoid[Set[A]] {
    def append(f1: Set[A], f2: => Set[A]) = f1 ++ f2
    def zero: Set[A] = Set[A]()
  }

  implicit def setShow[A: Show]: Show[Set[A]] = new Show[Set[A]] {
    override def show(as: Set[A]) = Cord("Set(", Cord.mkCord(",", as.map(Show[A].show).toSeq:_*), ")")
  }

}

trait SetFunctions

object set extends SetInstances with SetFunctions
