// Copyright Tony Morris 2008-2009
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.memo

import scala.collection.mutable.Map

/**
 * Represents any association from a <code>K</code> (key) to <code>V</code> (value) over the mutable type
 * <code>T</code>, which represents the association.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait MutableAssociation[K, V, T] {
  /**
   * Returns <code>Some</code> association with <code>k</code> on <code>t</code> or <code>None</code> if no such
   * association exists.
   *
   * @param t The mutable associative type to obtain the association from.
   * @param k The key for the association to obtain.
   * @return <code>Some</code> association with <code>k</code> or <code>None</code> if no such association exists.
   * @see <code><a href="#get%28T%29">get</a></code>.
   */
  def apply(t: T)(k: => K) = get(t)(k)

  /**
   * Returns <code>Some</code> association with <code>k</code> on <code>t</code> or <code>None</code> if no such
   * association exists.
   *
   * @param t The mutable associative type to obtain the association from.
   * @param k The key for the association to obtain.
   * @return <code>Some</code> association with <code>k</code> or <code>None</code> if no such association exists.
   */
  def get(t: T)(k: => K): Option[V]

  /**
   * Inserts the given <code>k</code> association with <code>v</code> into <code>t</code>.
   *
   * @param t The mutable associative type to insert the association into.
   * @param k The key for the association.
   * @param v The value for the association.
   */
  def insertKeyValue(t: T)(k: => K)(v: => V): Unit
}

/**
 * Functions creating mutable associative types.
 * 
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object MutableAssociation {
  /**
   * A mutable association.
   *
   * @return an association over mutable <code>Array</code>s.
   */
  implicit def ArrayAssociation[V] = new MutableAssociation[Int, V, Array[V]] {
    override def get(t: Array[V])(k: => Int) = if(t(k) == null) None else Some(t(k))
    override def insertKeyValue(t: Array[V])(k: => Int)(v: => V) = t(k) = v
  }
  
  /**
   * A mutable association.
   *
   * @return an association over mutable <code>Map</code>s.
   */
  implicit def MapAssociation[K, V] = new MutableAssociation[K, V, Map[K, V]] {
    override def get(t: Map[K, V])(k: => K) = t.get(k)
    override def insertKeyValue(t: Map[K, V])(k: => K)(v: => V) = t.update(k, v)
  }
}
