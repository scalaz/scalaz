// Copyright Tony Morris 2008-2009
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalazdemo.memo;

import scalaz.memo.Memo
import scalaz.memo.Memo._
import scalaz.memo.SizedMemo
import scalaz.memo.SizedMemo._

/**
 * An example of the use of the memoisation library. The example is around the edit distance problem, where a function
 * determines the minumum number of 'edits' of two sequences to convert the first into the second. For example, the
 * sequence <code>algorithm</code> to <code>altruistic</code> has a minimum edit distance of <code>6</code> because
 * there is no shorter way to convert <code>algorithm</code> into <code>altruistic</code> than:
 * <ul>
 * <li><code>Copy, Copy, Delete, Substitute(t), Copy, Insert(u), Copy, Insert(s), Copy, Substitute(i), Substitute(c)</code></li>
 * </ul>
 * Appeareances of <code>Copy</code> are weighted as <code>0</code>, leaving 6 edits required to transform the sequence.
 * <br><br>
 * Note that for this particular example, there are two other possibilities for transforming one to the other, also of
 * an edit distance of <code>6</code>.
 * <ul>
 * <li><code>Copy, Copy, Substitute(t), Delete, Copy, Insert(u), Copy, Insert(s), Copy, Substitute(i), Substitute(c)</code></li>
 * <li><code>Copy, Copy, Substitute(t), Substitute(r), Substitute(u), Copy, Insert(s), Copy, Substitute(i), Substitute(c)</code></li>
 * </ul>
 * <br><br>
 * The edit distance problem is a well known Dynamic Programming Algorithm (DPA) for which memoisation is very suitable.
 * This is evident by attempting to write the same algorithm some other way and observing the result. 
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object EditDistance {
  /**
   * Returns a dynamic matrix for backtracking and computing the edit distance of the two given arrays and uses the
   * given memoisation technique for building the matrix.
   *
   * @param im The sized memoisation technique for building the backtracking matrix.
   * @param x The array to transform and calculate the edit distance.
   * @param y The array to be transformed to and calculate the edit distance.
   * @return A dynamic matrix for backtracking.
   */
  def matrix1[A](im: SizedMemo[Int])(x: Array[A])(y: Array[A]): Int => Int => Int = {
    val m = im[Memo[Int, Int]](x.length + 1)

    def get(i: Int)(j: Int): Int = if(i == 0) j else if(j == 0) i else {
      lazy val t = x(i - 1)
      lazy val u = y(j - 1)
      lazy val e = t == u

      def f = (n: Int) => im[Int](y.length + 1)
      def a = m(f)(i - 1)(get(i - 1))(j) + 1
      def b = m(f)(i - 1)(get(i - 1))(j - 1) + (if(e) 0 else 1)
      def c = m(f)(i)(get(i))(j - 1) + 1

      if(a < b) a else if(b <= c) b else c
    }
    get
  }

  /**
   * Returns a dynamic matrix for backtracking and computing the edit distance of the two given arrays. The memoisation
   * technique is specified within the algorithm. Note that the signature is the same as
   * <a href="#matrixNoMemo%28Array%5Ba%5D%29">that without any memoisation</a>.
   *
   * @param x The array to transform and calculate the edit distance.
   * @param y The array to be transformed to and calculate the edit distance.
   * @return A dynamic matrix for backtracking.
   */
  def matrix2[A](x: Array[A])(y: Array[A]): Int => Int => Int = {
    val im = arraySizedMemo
    val m = im[Memo[Int, Int]](x.length + 1)

    def get(i: Int)(j: Int): Int = if(i == 0) j else if(j == 0) i else {
      lazy val t = x(i - 1)
      lazy val u = y(j - 1)
      lazy val e = t == u

      def f = (n: Int) => im[Int](y.length + 1)
      def a = m(f)(i - 1)(get(i - 1))(j) + 1
      def b = m(f)(i - 1)(get(i - 1))(j - 1) + (if(e) 0 else 1)
      def c = m(f)(i)(get(i))(j - 1) + 1

      if(a < b) a else if(b <= c) b else c
    }
    get
  }

  /**
   * Returns a dynamic matrix for backtracking and computing the edit distance of the two given arrays using <b>NO</b>
   * memoisation. This will result in a very poorly performing algorithm as it repeats computations without storing
   * them.
   *
   * @param x The array to transform and calculate the edit distance.
   * @param y The array to be transformed to and calculate the edit distance.
   * @return A matrix for backtracking.
   */
  def matrixNoMemo[A](x: Array[A])(y: Array[A]): Int => Int => Int = {
    def get(i: Int)(j: Int): Int = if(i == 0) j else if(j == 0) i else {
      lazy val t = x(i - 1)
      lazy val u = y(j - 1)
      lazy val e = t == u

      def a = get(i - 1)(j) + 1
      def b = get(i - 1)(j - 1) + (if(e) 0 else 1)
      def c = get(i)(j - 1) + 1
      if(a < b) a else if(b <= c) b else c
    }
    get
  }

  /**
   * Computes the edit distance of the two given arrays using the given backtracking matrix.
   *
   * @param f The backtracking matrix, to be used for computing the edit distance.
   * @param x The array to transform and calculate the edit distance.
   * @param y The array to be transformed to and calculate the edit distance.
   * @return The minimum edit distance from <code>x</code> to <code>y</code>
   */
  def editDistance[A](f: Int => Int => Int)(x: Array[A])(y: Array[A]) = f(x.length)(y.length)

  def main(args: Array[String]) = {
    val x = args(0).toCharArray
    val y = args(1).toCharArray
    
    println("x: " + args(0))
    println("y: " + args(1))
    println("--------")
    
    // passing the memoisation techniques
    println("editDistance(matrix1(arraySizedMemo)(x)(y))(x)(y): " + editDistance(matrix1(arraySizedMemo)(x)(y))(x)(y))
    println("editDistance(matrix1(mutableHashMapSizedMemo)(x)(y))(x)(y): " + editDistance(matrix1(mutableHashMapSizedMemo)(x)(y))(x)(y))
    println("editDistance(matrix1(immutableEmptyMapSizedMemo)(x)(y))(x)(y): " + editDistance(matrix1(immutableEmptyMapSizedMemo)(x)(y))(x)(y))
    println("editDistance(matrix1(immutableHashMapSizedMemo)(x)(y))(x)(y): " + editDistance(matrix1(immutableHashMapSizedMemo)(x)(y))(x)(y))
    println("editDistance(matrix1(immutableListMapSizedMemo)(x)(y))(x)(y): " + editDistance(matrix1(immutableListMapSizedMemo)(x)(y))(x)(y))
    println("editDistance(matrix1(immutableTreeMapSizedMemo)(x)(y))(x)(y): " + editDistance(matrix1(immutableTreeMapSizedMemo)(x)(y))(x)(y))
    println("editDistance(matrix1(immutableUnbalancedTreeMapSizedMemo)(x)(y))(x)(y): " + editDistance(matrix1(immutableUnbalancedTreeMapSizedMemo)(x)(y))(x)(y))

    // using the function-specified memoisation technique
    // matrix2 looks the same as the call to matrixNoMemo because they have the same signature
    println("editDistance(matrix2(x)(y))(x)(y): " + editDistance(matrix2(x)(y))(x)(y))

    // expect this to take a long time for any considerable value for the lengths of x and/or y
    println("editDistance(matrix1(nilSizedMemo)(x)(y))(x)(y): " + editDistance(matrix1(nilSizedMemo)(x)(y))(x)(y))
    // ...and this too
    println("editDistance(matrixNoMemo(x)(y))(x)(y): " + editDistance(matrixNoMemo(x)(y))(x)(y))
  }
}
