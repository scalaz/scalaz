// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.memo

import scala.collection.mutable.HashMap
import scala.collection.immutable.EmptyMap
import scala.collection.immutable.ListMap
import scala.collection.immutable.TreeMap
import scala.collection.immutable.UnbalancedTreeMap
import TMemo.mutableAssociationTMemo
import TMemo.immutableAssociationTMemo

/**
 * An abstraction representing memoisation used in many dynamic programming (DPA) algorithms and code that makes efforts
 * to follow a purely functional style. Loosely inspired by
 * <a href="http://research.microsoft.com/Users/simonpj/Papers/weak.ps.gz">Stretching the storage manager: weak pointers
 *  and stable names in Haskell (Simon Peyton Jones, Simon Marlow, and Conal Elliott, IFL'99.)</a>
 * Further abstractions, such as purging the memoisation table, are achieved by abstracting over the type that maintains
 * the assocation (and so, may define means of purging themselves).
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait Memo[K, V] {
  /**
   * Given a function <code>f</code> and an argument to be applied <code>k</code>, return the result of such an
   * application. Implementations are free (encouraged) to store the computation for future invocations. It is paramount
   * for function <code>f</code> to <strong>NOT</strong> be side-effecting and maintain referential transparency.
   *
   * @param f The function to apply to <code>k</code>.
   * @param k The argument to pass to function <code>f</code>.
   * @return The result of applying function <code>f</code> to <code>k</code>, under the assumption that <code>f</code>
   * is free of side-effects.
   * @see <code><a href="#get%28%28K%29%3D%3EV%29">get</a></code>
   */
  def apply(f: K => V)(k: => K): V = get(f)(k)

  /**
   * Given a function <code>f</code> and an argument to be applied <code>k</code>, return the result of such an
   * application. Implementations are free (encouraged) to store the computation for future invocations. It is paramount
   * for function <code>f</code> to <strong>NOT</strong> be side-effecting and maintain referential transparency.
   *
   * @param f The function to apply to <code>k</code>.
   * @param k The argument to pass to function <code>f</code>.
   * @return The result of applying function <code>f</code> to <code>k</code>, under the assumption that <code>f</code>
   * is free of side-effects.
   */
  def get(f: K => V)(k: => K): V
}

/**
 * Functions creating memoisation types.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Memo {
  /**
   * A memoisation technique.
   *
   * @return a memoisation technique where each application explicitly invokes the memo function. No attempt to store
   * the computation is made.
   */
  def nilMemo[K, V] = new Memo[K, V] {
    override def get(f: K => V)(k: => K) = f(k)
  }

  /**
   * A memoisation technique backed with an <code>Array</code>.
   *
   * @param size The size of the array to back the memoisation table with. An attempts to perform lookups outside of the
   * range is undefined.
   * @return a memoisation technique where each application is first looked up in the array at the given index to the
   * memo function and if already computed, returns the associated value. Otherwise, the memo function is invoked and
   * its value is stored in the mutable <code>Array</code>. An attempt to invoke the memo function outside of the range
   * of the array (<code>0 to size - 1 inclusive</code>) is non-terminating/undefined.
   */
  def arrayMemo[V](size: Int) = mutableAssociationTMemo(MutableAssociation.ArrayAssociation[V])(new Array[V](size))

  /**
   * A memoisation technique backed with a mutable <code>HashMap</code>.
   *
   * @return a memoisation technique where each application is first looked up in a mutable <code>HashMap</code> and if
   * already computed, returns the associated value. Otherwise, the memo function is invoked and its value is stored in
   * the mutable <code>HashMap</code>.
   */
  def mutableHashMapMemo[K, V] = mutableAssociationTMemo(MutableAssociation.MapAssociation[K, V])(new HashMap[K, V])

  /**
   * A memoisation technique backed with an immutable <code>EmptyMap</code>.
   *
   * @return a memoisation technique where each application is first looked up in the map and if already computed,
   * returns the associated value. Otherwise, the memo function is invoked and its value is stored in the immutable map.
   */
  def immutableEmptyMapMemo[K, V] = immutableAssociationTMemo(ImmutableAssociation.MapAssociation[K, V])(new EmptyMap[K, V])

  /**
   * A memoisation technique backed with an immutable <code>HashMap</code>.
   *
   * @return a memoisation technique where each application is first looked up in the map and if already computed,
   * returns the associated value. Otherwise, the memo function is invoked and its value is stored in the immutable map.
   */
  def immutableHashMapMemo[K, V] = immutableAssociationTMemo(ImmutableAssociation.MapAssociation[K, V])(new scala.collection.immutable.HashMap[K, V])

  /**
   * A memoisation technique backed with an immutable <code>ListMap</code>.
   *
   * @return a memoisation technique where each application is first looked up in the map and if already computed,
   * returns the associated value. Otherwise, the memo function is invoked and its value is stored in the immutable map.
   */
  def immutableListMapMemo[K, V] = immutableAssociationTMemo(ImmutableAssociation.MapAssociation[K, V])(new ListMap[K, V])

  /**
   * A memoisation technique backed with an immutable <code>TreeMap</code>.
   *
   * @return a memoisation technique where each application is first looked up in the map and if already computed,
   * returns the associated value. Otherwise, the memo function is invoked and its value is stored in the immutable map.
   */
  def immutableTreeMapMemo[K <% Ordered[K], V] = immutableAssociationTMemo(ImmutableAssociation.MapAssociation[K, V])(new TreeMap[K, V])

  /**
   * A memoisation technique backed with an immutable <code>UnbalancedTreeMap</code>.
   *
   * @return a memoisation technique where each application is first looked up in the map and if already computed,
   * returns the associated value. Otherwise, the memo function is invoked and its value is stored in the immutable map.
   */
  def immutableUnbalancedTreeMapMemo[K <% Ordered[K], V] = immutableAssociationTMemo(ImmutableAssociation.MapAssociation[K, V])(new UnbalancedTreeMap[K, V])
}
