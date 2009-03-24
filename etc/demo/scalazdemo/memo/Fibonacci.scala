// Copyright Tony Morris 2008-2009
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalazdemo.memo;

import scalaz.memo.Memo
import scalaz.memo.Memo._

/**
 * An example of the use of the memoisation library. The example is around the fibonacci series of numbers, where each
 * value is the sum of its two predecessors.
 * <br/>
 * <code>1, 1, 2, 3, 5, 8, 13, 21, 34, 55, 89, ...</code>
 * <hr>
 * Inductive definition (fib):
 * <pre>
fib 0 = 1
fib 1 = 1
fib n = fib (n - 1) + fib (n - 2)
 * </pre>
 * <i>Note that in the inductive case of the definition, there are overlapping computations. e.g. computing the
 * <code>(n - 1)th</code> fibonacci number will compute the <code>(n - 2)th</code> fibonacci, which is also computed
 * again later (and so on). This algorithm has an excessive time complexity that is solved by memoising during
 * computation.</i>
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Fibonacci {
  /**
   * Computes the <code>nth</code> fibonacci number by accepting the memoisation technique to use during computation.
   *
   * @param m The memoisation technique.
   * @param n The index of the fibonacci number to compute.
   * @return The <code>nth</code> fibonacci number.
   */
  def fib1(m: Memo[Int, Int])(n: Int): Int = n match {
    case 0 => 1
    case 1 => 1
    case x if x > 1 => m(fib1(m) _)(n - 1) + m(fib1(m) _)(n - 2)
  }

  /**
   * Computes the <code>nth</code> fibonacci number by using a local memoisation technique. In this case, the
   * memoisation technique is backed by an array of size <code>n</code>.
   * Note that the function signature (<code>Int -> Int</code>) is the same as that of the
   * <a href="#fibNoMemo%28Int%29">fibNoMemo</a> method.
   *
   * @param n The index of the fibonacci number to compute.
   * @return The <code>nth</code> fibonacci number.
   */
  def fib2(n: Int): Int = {
    val m = arrayMemo[Int](n)

    def fib(n: Int): Int = n match {
      case 0 => 1
      case 1 => 1
      case x if x > 1 => m(fib _)(n - 1) + m(fib _ )(n - 2) 
    }

    fib(n)
  }

  /**
   * Computes the <code>nth</code> fibonacci number without any memoisation. This algorithm should be expected to take
   * an unreasonable amount of time for any considerable value for x (try <code>50</code> for example). This is because
   * of the redundant computations that take place without storing previous computation results.
   *
   * @param n The index of the fibonacci number to compute.
   * @return The <code>nth</code> fibonacci number.
   */
  def fibNoMemo(n: Int): Int = n match {
    case 0 => 1
    case 1 => 1
    case x if x > 1 => fibNoMemo(n - 1) + fibNoMemo(n - 2)
  }

  /**
   * Calls the <code>fib*</code> methods with the first command line argument (subsequent arguments are ignored) and
   * prints their results to the standard output stream.
   *
   * @param args The command line arguments (only the first is used).
   */
  def main(args: Array[String]) = {
    val x = java.lang.Integer.parseInt(args(0))
    import Console.println

    // passing the memoisation techniques
    println("fib1(arrayMemo[Int](x))(x): " + fib1(arrayMemo[Int](x))(x))
    println("fib1(immutableEmptyMapMemo[Int, Int])(x): " + fib1(immutableEmptyMapMemo[Int, Int])(x))
    println("fib1(immutableHashMapMemo[Int, Int])(x): " + fib1(immutableHashMapMemo[Int, Int])(x))
    println("fib1(immutableListMapMemo[Int, Int])(x): " + fib1(immutableListMapMemo[Int, Int])(x))
    println("fib1(immutableTreeMapMemo[Int, Int])(x): " + fib1(immutableTreeMapMemo[Int, Int])(x))
    println("fib1(immutableUnbalancedTreeMapMemo[Int, Int])(x): " + fib1(immutableUnbalancedTreeMapMemo[Int, Int])(x))
    println("fib1(mutableHashMapMemo[Int, Int])(x): " + fib1(mutableHashMapMemo[Int, Int])(x))

    // using the function-specified memoisation technique
    // fib2 looks the same as the call to fibNoMemo because they have the same signature
    println("fib2(x): " + fib2(x))

    
    // expect this to take a long time for any considerable value for x
    println("fib1(nilMemo[Int, Int])(x): " + fib1(nilMemo[Int, Int])(x))
    // ...and this too
    println("fibfibNoMemo(x): " + fibNoMemo(x))
  }
}

