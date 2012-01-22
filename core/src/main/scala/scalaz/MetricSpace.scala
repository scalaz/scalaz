package scalaz

////
/**
 * Useful metric spaces include the manhattan distance between two points,
 * the Levenshtein edit distance between two strings, the number of
 * edges in the shortest path between two nodes in an undirected graph
 * and the Hamming distance between two binary strings. Any euclidean
 * space also has a metric. However, in this module we use int-valued
 * metrics and that's not compatible with the metrics of euclidean
 * spaces which are real-values.
 *
 * @see [[scalaz.BKTree]]
 */
////
trait MetricSpace[F]  { self =>
  ////
  def distance(a: F, b: F): Int

  def contramap[B](f: B => F): MetricSpace[B] = new MetricSpace[B] {
    def distance(a: B, b: B): Int = self.distance(f(a), f(b))
  }

  // derived functions

  trait MetricSpaceLaw {

    import std.boolean.conditional

    def nonNegativity(a1: F, a2: F): Boolean = distance(a1, a1) >= 0
    def identity(a1: F): Boolean = distance(a1, a1) == 0
    def equality(a1: F, a2: F)(implicit F: Equal[F]): Boolean = conditional(F.equal(a1, a2), distance(a1, a2) == 0)
    def symmetry(a1: F, a2: F): Boolean = distance(a1, a2) == distance(a2, a1)
    def triangleInequality(a1: F, a2: F, a3: F): Boolean = (distance(a1, a2) + distance(a2, a3)) >= distance(a1, a3)
  }

  def metricSpaceLaw = new MetricSpaceLaw {}

  ////
  val metricSpaceSyntax = new scalaz.syntax.MetricSpaceSyntax[F] {}
}

object MetricSpace {
  @inline def apply[F](implicit F: MetricSpace[F]): MetricSpace[F] = F

  ////
  val metricSpaceInstance = new Contravariant[MetricSpace] {
    def contramap[A, B](r: MetricSpace[A])(f: (B) => A): MetricSpace[B] = r contramap f
  }

  def metricSpace[A](f: (A, A) => Int): MetricSpace[A] = new MetricSpace[A] {
    def distance(a1: A, a2: A): Int = f(a1, a2)
  }

  def levenshtein[F[_], A](implicit l: Length[F], i: Index[F], e: Equal[A]): MetricSpace[F[A]] = new MetricSpace[F[A]] {
    def distance(a1: F[A], a2: F[A]): Int = levenshteinDistance(a1, a2)
  }

  def levenshteinDistance[F[_], A](value: F[A], w: F[A])(implicit l: Length[F], ind: Index[F], equ: Equal[A]): Int = {
    import Memo._
    def levenshteinMatrix(w: F[A])(implicit l: Length[F], ind: Index[F], equ: Equal[A]): (Int, Int) => Int = {
      val m = mutableHashMapMemo[(Int, Int), Int]

      def get(i: Int, j: Int): Int = if (i == 0) j
      else if (j == 0) i
      else {
        lazy val t: A = ind.index(value, (i - 1)).get
        lazy val u: A = ind.index(w, (j - 1)).get
        lazy val e: Boolean = equ.equal(t, u)


        val g: ((Int, Int)) => Int = m {
          case (a, b) => get(a, b)
        }
        val a: Int = g((i - 1, j)) + 1
        val b: Int = g((i - 1, j - 1)) + (if (e) 0 else 1)
        def c: Int = g((i, j - 1)) + 1
        if (a < b) a else if (b <= c) b else c
      }

      get
    }

    val k = levenshteinMatrix(w)
    k(l.length(value), l.length(w))
  }

  implicit def LevenshteinString: MetricSpace[String] = {
    import std.list._, std.anyVal._
    levenshtein[List, Char].contramap((s: String) => s.toList)
  }
  ////
}

