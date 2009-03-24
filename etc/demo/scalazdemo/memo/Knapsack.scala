// Copyright Tony Morris 2008-2009
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalazdemo.memo;

case class Item(size: Int, value: Int)

import java.lang.Math.max
import scalaz.memo.Memo
import scalaz.memo.Memo._
import scalaz.memo.SizedMemo
import scalaz.memo.SizedMemo._

/**
 * An example of the use of the memoisation library. The example is around the 0-1 knapsack problem. The problem may be 
 * stated as follows. A thief enters a building with a knapsack that has a capacity of 6 (arbitrary units of 
 * measurement). There are 4 items in the building each of various sizes and values. The thief wants to maximise his 
 * taking without exceeding his knapsack limit. The four items have sizes and values:
 * <li>Size 3 Value 5</li>
 * <li>Size 2 Value 3</li>
 * <li>Size 1 Value 1</li>                          
 * <li>Size 4 Value 7</li>
 * <p>
 * A greedy algorithm might take items while it can, so takes the first, second and third item to find that the knapsack
 * is full with a total value of 9 (5 + 3 + 1), however, a more clever thief would take the second and fourth items with
 * a total value of 10 (3 + 7). This would require the thief to apply the dynamic programming solution with 
 * backtracking.
 * </p>
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Knapsack {
   /**
    * Returns a dynamic matrix for backtracking and computing the optimum items to take.
    *
    * @param im The sized memoisation technique for building the backtracking matrix.
    * @param items All items from which to select.
    * @param knapsack The capacity of the knapsack.
    * @return A dynamic matrix for backtracking.
    */
  def matrix1(im: SizedMemo[Int])(items: Array[Item], knapsack: Int): Int => Int => Int = {
    val m = im[Memo[Int, Int]](items.length)
    
    def get(i: Int)(j: Int): Int = {
      def f = (n: Int) => im[Int](knapsack + 1)
      lazy val s = items(i - 1).size
      lazy val v = items(i - 1).value
      
      if(i == 0 || j == 0) 
        0 
      else if(j < s)
        m(f)(i - 1)(get(i - 1))(j)
      else
        max(m(f)(i - 1)(get(i - 1))(j), if(j == s) v else v + m(f)(i - 1)(get(i - 1))(j - s))      
    }
    
    get
  }
  
  /**
   * Returns a dynamic matrix for backtracking and computing the optimum items to take. The memoisation technique is
   * specified within the algorithm. Note that the signature is the same as
   * <a href="#matrixNoMemo%28Array%5Bcom.workingmouse.memo.example.Item%5D%2CInt%29">that without any memoisation</a>.
   *
   * @param items All items from which to select.
   * @param knapsack The capacity of the knapsack.   
   * @return A dynamic matrix for backtracking.
   */
  def matrix2(items: Array[Item], knapsack: Int): Int => Int => Int = {
    val im = arraySizedMemo
    val m = im[Memo[Int, Int]](items.length)
    
    def get(i: Int)(j: Int): Int = {
      def f = (n: Int) => im[Int](knapsack + 1)
      lazy val s = items(i - 1).size
      lazy val v = items(i - 1).value
      
      if(i == 0 || j == 0) 
        0 
      else if(j < s)
        m(f)(i - 1)(get(i - 1))(j)
      else
        max(m(f)(i - 1)(get(i - 1))(j), if(j == s) v else v + m(f)(i - 1)(get(i - 1))(j - s))      
    }
    
    get
  }  
  
  /**
   * Returns a dynamic matrix for backtracking and computing the optimum items to take using <b>NO</b> memoisation.
   *
   * @param im The sized memoisation technique for building the backtracking matrix.
   * @param items All items from which to select.
   * @param knapsack The capacity of the knapsack.
   * @return A dynamic matrix for backtracking.
   */
  def matrixNoMemo(items: Array[Item], knapsack: Int): Int => Int => Int = {
    def get(i: Int)(j: Int): Int = {
      lazy val s = items(i - 1).size
      lazy val v = items(i - 1).value
      
      if(i == 0 || j == 0) 
        0 
      else if(j < s)
        get(i - 1)(j)
      else
        max(get(i - 1)(j), if(j == s) v else v + get(i - 1)(j - s))      
    }
    
    get
  }

  /**
   * Backtrack the given matrix for the given items and return which items to take to maximise the value.
   *
   * @param f The matrix to backtrack.
   * @param items All items from which to select.
   * @param j The capacity of the knapsack used to create the matrix.
   * @return The optimum items to select to maximise value.
   */
  def optimumItems(f: Int => Int => Int, items: Array[Item], j: Int) = {
    def backtrack(i: Int, j: Int): List[Item] = {     
      if(i == 0 && j == 0)
        Nil
      else if(f(i)(j) == f(i - 1)(j))
        backtrack(i - 1, j)
      else 
        items(i - 1) :: backtrack(i - 1, j - items(i - 1).size)
    }
    
    backtrack(items.length, j)
  }
  
  /**
   * Prints the optimum items to maximise the value of the given inputs.
   *
   * @param args An odd number of integer values where the first n - 1 arguments represent the item's size and value and 
   * the last argument representing the knapsack capacity.
   * <p>
   * For example, <code>3 5 2 3 1 1 4 7 6</code> produces 4 items
   * <li>size: 3 value: 5</li>
   * <li>size: 2 value: 3</li>
   * <li>size: 1 value: 1</li>
   * <li>size: 4 value: 7</li>
   * ...and a knapsack capacity of 6
   * </p>
   */
  def main(args: Array[String]) = {
    def itemArgs(xs: List[Int]): (List[Item], Int) = xs match {
      case Nil => (Nil, 0)
      case a :: Nil => (Nil, a)
      case x :: y :: ys => itemArgs(ys) match {
        case (items, knapsack) => (Item(x, y) :: items, knapsack) 
      }
    }
    val parseArgs = itemArgs(args.map(java.lang.Integer.parseInt(_)).toList)
    val items = parseArgs._1.toArray
    val knapsack = parseArgs._2
    
    println("items: " + parseArgs._1)
    println("knapsack size: " + knapsack)
    val m = matrix2(items, knapsack)
    val o = optimumItems(m, items, knapsack)
    println("optimum items: " + o)
    println("optimum value         : " + m(items.length)(knapsack))
    println("optimum value (verify): " + o.foldLeft(0)((x, i) => x + i.value))
  }
}
