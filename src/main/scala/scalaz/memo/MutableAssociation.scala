package scalaz.memo

import scala.collection.mutable.Map

trait MutableAssociation[-T, K, V] {
  def apply(t: T, k: K): Option[V]

  def insert(t: T, k: K, v: V): Unit

  def comemo = Comemo.comemo[T, K, V](t => Memo.memo[K, V](f => k => {
    this(t, k) match {
      case Some(v) => v
      case None => {
        val x = f(k)
        insert(t, k, x)
        x
      }
    }
  }))
}

object MutableAssociation {
  implicit def ArrayMutableAssociation[V] = new MutableAssociation[Array[V], Int, V] {
    def apply(t: Array[V], k: Int) = if(t(k) == null) None else Some(t(k))
    def insert(t: Array[V], k: Int, v: V) = t(k) = v
  }
  
  implicit def MapMutableAssociation[K, V] = new MutableAssociation[Map[K, V], K, V] {
    def apply(t: Map[K, V], k: K) = t.get(k)
    def insert(t: Map[K, V], k: K, v: V) = t.update(k, v)
  }
}
