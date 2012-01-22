package scalaz
package std

trait SetInstances {
  implicit val setInstance = new Traverse[Set] with MonadPlus[Set] with Each[Set] with Length[Set] {
    def each[A](fa: Set[A])(f: (A) => Unit) = fa foreach f
    def length[A](fa: Set[A]) = fa.size
    def point[A](a: => A) = Set(a)
    def bind[A, B](fa: Set[A])(f: A => Set[B]) = fa flatMap f
    def empty[A] = Set()
    def plus[A](a: Set[A], b: => Set[A]) = a ++ b
    override def map[A, B](l: Set[A])(f: A => B) = l map f

    // TODO duplication with ListInstances
    def traverseImpl[F[_], A, B](l: Set[A])(f: A => F[B])(implicit F: Applicative[F]) = {
      DList.fromList(l.toList).foldr(F.point(Set[B]())) {
        (a, fbs) => F.map2(f(a), fbs)((a, b) => b + a)
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

  /**
   * We could derive set equality from `Equal[A]`, but it would be `O(n^2)`.
   * Instead, we require `Order[A]`, reducing the complexity to `O(log n)`
   *
   * If `Equal[A].equalIsNatural == true`, than `Any#==` is used.
   */
  implicit def setEqual[A: Order]: Equal[Set[A]] = new Equal[Set[A]] {
    def equal(a1: Set[A], a2: Set[A]) = {
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
}

trait SetFunctions

object set extends SetInstances with SetFunctions
