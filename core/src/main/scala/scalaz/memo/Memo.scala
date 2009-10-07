package scalaz.memo

import scala.collection.mutable.HashMap
import scala.collection.immutable.EmptyMap
import scala.collection.immutable.ListMap
import scala.collection.immutable.TreeMap
import scala.collection.immutable.UnbalancedTreeMap

trait Memo[K, V] {
  def apply(z: K => V): K => V

  def const[T] = Comemo.comemo[T, K, V](_ => this)
}

object Memo {
  def memo[K, V](f: (K => V) => K => V) = new Memo[K, V] {
    def apply(z: K => V) = f(z)
  }
  
  def nilMemo[K, V] = memo[K, V](z => z)

  import MutableAssociation._
  import ImmutableAssociation._

  def mutableHashMapMemo[K, V] = MapMutableAssociation.comemo(new HashMap[K, V])

  def immutableEmptyMapMemo[K, V] = ImmutableMapAssociation.comemo(new EmptyMap[K, V])

  def immutableHashMapMemo[K, V] = ImmutableMapAssociation.comemo(new scala.collection.immutable.HashMap[K, V])

  def immutableListMapMemo[K, V] = ImmutableMapAssociation.comemo(new ListMap[K, V])

  def immutableListMapMemo[K <% Ordered[K], V] = ImmutableMapAssociation.comemo(new TreeMap[K, V])

  def immutableUnbalancedTreeMapMemo[K <% Ordered[K], V] = ImmutableMapAssociation.comemo(new UnbalancedTreeMap[K, V])
}
