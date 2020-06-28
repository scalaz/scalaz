package scalaz
package std

trait SetInstances {
  implicit val setInstance: Foldable[Set] with IsEmpty[Set] = new Foldable[Set] with IsEmpty[Set] with Foldable.FromFoldr[Set] with IterableSubtypeFoldable[Set] {
    override def length[A](fa: Set[A]) = fa.size
    def empty[A] = Set()
    def plus[A](a: Set[A], b: => Set[A]) = a ++ b
    def isEmpty[A](fa: Set[A]) = fa.isEmpty

    override def foldRight[A, B](fa: Set[A], z: => B)(f: (A, => B) => B) = {
      import scala.collection.mutable.ArrayStack
      val s = new ArrayStack[A]
      fa.foreach(a => s push a)
      var r = z
      while (!s.isEmpty) {
        // Fixes stack overflow issue (#866)
        val w = r
        r = f(s.pop(), w)
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
      implicit val o: scala.Ordering[A] = Order[A].toScalaOrdering
      implicit val so: Order[Seq[A]] = Order.fromScalaOrdering(implicitly[scala.math.Ordering[Seq[A]]])
      Order[Int].order(a1.size, a2.size) match {
        case EQ => Order.orderBy((s: Set[A]) => s.toSeq.sorted).order(a1, a2)
        case x => x
      }
    }

    override def equal(a1: Set[A], a2: Set[A]) = {
      if (equalIsNatural) a1 == a2
      else {
        implicit val x: scala.Ordering[A] = Order[A].toScalaOrdering
        import scala.collection.immutable.TreeSet
        val s1 = TreeSet[A](a1.toSeq: _*)
        val s2 = TreeSet[A](a2.toSeq: _*)
        s1.iterator.corresponds(s2.iterator)(Order[A].equal)
      }
    }
    override val equalIsNatural: Boolean = Equal[A].equalIsNatural
  }

  implicit def setMonoid[A]: Monoid[Set[A]] with SemiLattice[Set[A]] =
    new Monoid[Set[A]] with SemiLattice[Set[A]] {
      def append(f1: Set[A], f2: => Set[A]) = f1 ++ f2
      def zero: Set[A] = Set[A]()
    }

  implicit def setShow[A](implicit A: Show[A]): Show[Set[A]] = Show.show { as =>
    import scalaz.syntax.show._
    val content = Foldable[Set].intercalate(as.map(A.show), Cord(","))
    cord"Set($content)"
  }

}

trait SetFunctions

object set extends SetInstances with SetFunctions
