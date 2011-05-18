package scalaz

/**
 * <ol>
 * <li>(distance(a, b) == 0) == (a == b)</li>
 * <li>Commutativity: distance(a, b) == distance(b, a)</li>
 * <li>Triangle Equality: distance(a, b) + distance(b, c) >= distance(a, c)</li>
 * </ol>
 */
sealed trait MetricSpace[A] {
  val distance: A => A => Int
}

trait MetricSpaces {
  def metricSpace[A](f: A => A => Int): MetricSpace[A] = new MetricSpace[A] {
    val distance = f
  }

}