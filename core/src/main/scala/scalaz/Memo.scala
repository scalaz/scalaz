package scalaz

sealed trait Memo[@specialized(Int) K, @specialized(Int, Long, Double) V] {
  def apply(z: K => V): K => V
}

trait Memos {
  def memo[@specialized(Int) K, @specialized(Int, Long, Double) V](f: (K => V) => K => V): Memo[K, V] = new Memo[K, V] {
    def apply(z: K => V) = f(z)
  }

  def nilMemo[@specialized(Int) K, @specialized(Int, Long, Double) V]: Memo[K, V] = memo[K, V](z => z)

  private class ArrayMemo[V >: Null: ClassManifest](n: Int) extends Memo[Int, V] {
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

  def arrayMemo[V >: Null : ClassManifest](n: Int): Memo[Int, V] = new ArrayMemo(n)
  def doubleArrayMemo(n: Int, sentinel: Double = 0d): Memo[Int, Double] = new DoubleArrayMemo(n, sentinel)

  def mutableHashMapMemo[K, V]: Memo[K, V] = {
    val a = new collection.mutable.HashMap[K, V]

    memo[K, V](f =>
      k =>
        a get k getOrElse {
          val v = f(k)
          a update (k, v)
          v
        })
  }

  def weakHashMapMemo[K, V]: Memo[K, V] = {
    val a = new java.util.WeakHashMap[K, V]

    memo[K, V](f =>
      k => {
        val v = a get k
        if (v == null) {
          val nv = f(k)
          a put (k, nv)
          nv
        } else {
          v
        }
      })
  }

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

  def immutableHashMapMemo[K, V]: Memo[K, V] = immutableMapMemo(new HashMap[K, V])

  def immutableListMapMemo[K, V]: Memo[K, V] = immutableMapMemo(new ListMap[K, V])

  def immutableTreeMapMemo[K: scala.Ordering, V]: Memo[K, V] = immutableMapMemo(new TreeMap[K, V])
}
