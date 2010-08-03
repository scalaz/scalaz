package scalaz

sealed trait Memo[K, V] {
  def apply(z: K => V): K => V
}

trait Memos {
  def memo[K, V](f: (K => V) => K => V): Memo[K, V] = new Memo[K, V] {
    def apply(z: K => V) = f(z)
  }

  def nilMemo[K, V]: Memo[K, V] = memo[K, V](z => z)

  def arrayMemo[V: Manifest](n: Int): Memo[Int, V] = {
    val a = new Array[V](n)

    memo[Int, V](f =>
      k => {
        val t = a(k)
        if (t == null) {
          val v = f(k)
          a(k) = v
          v
        } else t
      })
  }

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

  def immutableTreeMapMemo[K <: Ordered[K], V]: Memo[K, V] = immutableMapMemo(new TreeMap[K, V])
}
