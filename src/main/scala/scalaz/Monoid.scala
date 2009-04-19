package scalaz

trait Monoid[M] extends Zero[M] with Semigroup[M]

object Monoid {
  def monoid[M](implicit s: Semigroup[M], z: Zero[M]) = new Monoid[M] {
    def append(s1: M, s2: => M) = s append (s1, s2)
    val zero = z.zero
  }

  implicit val StringMonoid = monoid[String]
}
