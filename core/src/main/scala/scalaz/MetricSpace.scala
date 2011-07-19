package scalaz

/**
 * <ol>
 * <li>(distance(a, b) == 0) == (a == b)</li>
 * <li>Commutativity: distance(a)(b) == distance(b)(a)</li>
 * <li>Triangle Equality: distance(a)(b) + distance(b)(c) >= distance(a)(c)</li>
 * </ol>
 */
sealed trait MetricSpace[A] {
  val distance: A => A => Int

  def contramap[B](f: B => A): MetricSpace[B] =
    MetricSpace.metricSpace(b1 => b2 => MetricSpace.this.distance(f(b1))(f(b2)))
}

object MetricSpace extends MetricSpaces

trait MetricSpaces {
  def metricSpace[A](f: A => A => Int): MetricSpace[A] = new MetricSpace[A] {
    val distance = f
  }

  import *->*._

  def levenshtein[F[_], A](implicit l: Length[F], i: Index[F], e: Equal[A]): MetricSpace[F[A]] =
    metricSpace[F[A]](a => a <---> _)

  implicit def LevenshteinString: MetricSpace[String] =
    levenshtein[List, Char] ∙ ((s: String) => s.toList)

}