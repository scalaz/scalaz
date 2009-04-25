package scalaz.memo

import scala.collection.immutable.Map

trait ImmutableAssociation[T, K, V] {
  def apply(t: T, k: K): Option[V]

  def insert(t: T, k: K, v: V): T

  def comemo = Comemo.comemo[T, K, V](t => new Memo[K, V] {
    var tt = t
    def apply(f: K => V) = (k: K) => {
      val kk = k
      ImmutableAssociation.this(tt, kk) match {
        case Some(v) => v
        case None => {
          val x = f(kk)
          tt = insert(tt, kk, x)
          x
        }
      }
    }
  })
}

object ImmutableAssociation {
  implicit def ImmutableMapAssociation[K, V] = new ImmutableAssociation[Map[K, V], K, V] {
    def apply(t: Map[K, V], k: K) = t.get(k)
    def insert(t: Map[K, V], k: K, v: V) = t.update(k, v)
  }
}
