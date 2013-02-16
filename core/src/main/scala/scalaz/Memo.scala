package scalaz

/** A function memoization strategy.  See companion for various
  * instances employing various strategies.
  */
sealed trait Memo[@specialized(Int) K, @specialized(Int, Long, Double) V] {
  def apply(z: K => V): K => V
}

object Memo extends MemoFunctions with MemoInstances

trait MemoInstances {
}

/** @define immuMapNote As this memo uses a single var, it's
  * thread-safe. */
trait MemoFunctions {
  def memo[@specialized(Int) K, @specialized(Int, Long, Double) V](f: (K => V) => K => V): Memo[K, V] = new Memo[K, V] {
    def apply(z: K => V) = f(z)
  }

  def nilMemo[@specialized(Int) K, @specialized(Int, Long, Double) V]: Memo[K, V] = memo[K, V](z => z)

  private class ArrayMemo[V >: Null : ClassManifest](n: Int) extends Memo[Int, V] {
    override def apply(f: (Int) => V) = {
      lazy val a = new Array[V](n)
      k => {
        val t = a(k)
        if (t == null) {
          val v = f(k)
          a(k) = v
          v
        } else t
      }
    }
  }

  private class DoubleArrayMemo(n: Int, sentinel: Double) extends Memo[Int, Double] {
    override def apply(f: (Int) => Double) = {
      lazy val a = {
        if (sentinel == 0d) {
          new Array[Double](n)
        } else {
          Array.fill(n)(sentinel)
        }
      }
      k => {
        val t = a(k)
        if (t == sentinel) {
          val v = f(k)
          a(k) = v
          v
        } else t
      }
    }
  }

  /** Cache results in an `n`-long array. */
  def arrayMemo[V >: Null : ClassManifest](n: Int): Memo[Int, V] = new ArrayMemo(n)

  /** As with `arrayMemo`, but memoizing double results !=
    * `sentinel`.
    */
  def doubleArrayMemo(n: Int, sentinel: Double = 0d): Memo[Int, Double] = new DoubleArrayMemo(n, sentinel)

  private def mutableMapMemo[K, V](a: collection.mutable.Map[K, V]): Memo[K, V] =
    memo[K, V](f => k => a.getOrElseUpdate(k, f(k)))

  /** Cache results in a [[scala.collection.mutable.HashMap]].
    * Nonsensical if `K` lacks a meaningful `hashCode` and
    * `java.lang.Object.equals`.
    */
  def mutableHashMapMemo[K, V]: Memo[K, V] =
    mutableMapMemo(new collection.mutable.HashMap[K, V])

  /** As with `mutableHashMapMemo`, but forget elements according to
    * GC pressure.
    */
  def weakHashMapMemo[K, V]: Memo[K, V] =
    mutableMapMemo(new collection.mutable.WeakHashMap[K, V])


  private def immutableMapMemo[K, V](m: Map[K, V]): Memo[K, V] = {
    var a = m

    memo[K, V](f =>
      k => {
        a get k getOrElse {
          val v = f(k)
          a = a updated (k, v)
          v
        }
      })
  }

  import collection.immutable.{HashMap, ListMap, TreeMap}

  /** Cache results in a hash map.  Nonsensical unless `K` has
    * a meaningful `hashCode` and `java.lang.Object.equals`.
    * $immuMapNote
    */
  def immutableHashMapMemo[K, V]: Memo[K, V] = immutableMapMemo(new HashMap[K, V])

  /** Cache results in a list map.  Nonsensical unless `K` has
    * a meaningful `java.lang.Object.equals`.  $immuMapNote
    */
  def immutableListMapMemo[K, V]: Memo[K, V] = immutableMapMemo(new ListMap[K, V])

  /** Cache results in a tree map. $immuMapNote */
  def immutableTreeMapMemo[K: scala.Ordering, V]: Memo[K, V] = immutableMapMemo(new TreeMap[K, V])
}
