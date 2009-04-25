package scalaz

/*
1. (distance(a, b) == 0) == (a == b)
2. distance(a, b) == distance(b, a)
3. distance(a, b) + distance(b, c) >= distance(a, c)
 */
sealed trait MetricSpace[-A] {
  def distance(a1: A, a2: A): Int
}

object MetricSpace {
  def metricSpace[A](f: (A, A) => Int) = new MetricSpace[A] {
    def distance(a1: A, a2: A) = f(a1, a2)
  }

  import MA._

  def levenshtein[M[_], A](implicit l: Length[M], i: Index[M], e: Equal[A]) = metricSpace[M[A]]((a1, a2) => {
    implicit def XMA[A](a: M[A]) = MA.ma[M](a)
    a1 <---> a2
  })
}