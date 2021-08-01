package scalaz

sealed abstract class CaseInsensitive[A] {
  val original: A
  def foldedCase: A

  final def map[B: FoldCase](f: A => B): CaseInsensitive[B] = CaseInsensitive(f(original))

  final override def equals(other: Any): Boolean = other match {
    case that: CaseInsensitive[_] => foldedCase == that.foldedCase
    case _ => false
  }

  private[this] val hash = Need(foldedCase.hashCode)

  final override def hashCode: Int = hash.value
}

object CaseInsensitive extends CaseInsensitiveInstances {
  def apply[A](a: A)(implicit A: FoldCase[A]): CaseInsensitive[A] = mk(a, A.foldCase(a))

  private[scalaz] def mk[A](a: A, fc: => A): CaseInsensitive[A] = new CaseInsensitive[A] {
    val original = a
    private[this] val fcCache = Need(fc)
    def foldedCase = fcCache.value
  }
}

sealed abstract class CaseInsensitiveInstances {
  implicit def CaseInsensitiveMonoid[A: FoldCase : Monoid]: Monoid[CaseInsensitive[A]] =
    new Monoid[CaseInsensitive[A]] {
      def zero = CaseInsensitive.mk(Monoid[A].zero, Monoid[A].zero)
      def append(a: CaseInsensitive[A], b: => CaseInsensitive[A]) =
        CaseInsensitive.mk(Semigroup[A].append(a.original, b.original), Semigroup[A].append(a.foldedCase, b.foldedCase))
    }

  implicit def CaseInsensitiveEqual[A: Equal]: Equal[CaseInsensitive[A]] =
    (a: CaseInsensitive[A], b: CaseInsensitive[A]) => Equal[A].equal(a.foldedCase, b.foldedCase)

  implicit def CaseInsensitiveOrder[A: Order]: Order[CaseInsensitive[A]] =
    (a: CaseInsensitive[A], b: CaseInsensitive[A]) => Order[A].order(a.foldedCase, b.foldedCase)

  implicit def CaseInsensitiveShow[A: Show]: Show[CaseInsensitive[A]] =
    (a: CaseInsensitive[A]) => Show[A].show(a.original)
}

trait FoldCase[A] {
  def foldCase(a: A): A
}

object FoldCase extends FoldCaseInstances

sealed abstract class FoldCaseInstances {
  implicit val StringFoldCase: FoldCase[String] = (s: String) => s.toLowerCase
}
