// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.memo

/**
 * An association from some type to a memoisation technique backed by that type.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
trait TMemo[K, V, T] {
  /**
   * A memoisation technique backed by the given type. 
   *
   * @param t the type to back the memoisation technique with.    
   * @return a memoisation technique backed with the given type.
   * @see <code><a href="#tmemo%28T%29">get</a></code>
   */
  def apply(t: T) = tmemo(t)

  /**
   * A memoisation technique backed by the given type.
   *
   * @param t the type to back the memoisation technique with.
   * @return a memoisation technique backed with the given type.
   */
  def tmemo(t: T): Memo[K, V]
}

/**
 * Functions creating a type association to memoisation techniques.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object TMemo {
  /**
   * A type association to a memoisation technique using a type for which a mutable association exists.
   *
   * @param m the mutable association for the type to back the memoisation technique with.
   * @return a type association to a memoisation technique using a type for which a mutable association exists.
   */
  def mutableAssociationTMemo[K, V, T](implicit m: MutableAssociation[K, V, T]) = new TMemo[K, V, T] {
    override def tmemo(t: T) = new Memo[K, V] {
      override def get(f: K => V)(k: => K) = {
        val kk = k
        m(t)(kk) match {
          case Some(v) => v
          case None => {
            val x = f(kk)
            m.insertKeyValue(t)(kk)(x)
            x
          }
        }
      }
    }
  }

  /**
   * A type association to a memoisation technique using a type for which an immutable association exists.
   *
   * @param m the immutable association for the type to back the memoisation technique with.
   * @return a type association to a memoisation technique using a type for which an immutable association exists.
   */
  def immutableAssociationTMemo[K, V, T](implicit m: ImmutableAssociation[K, V, T]) = new TMemo[K, V, T] {
    override def tmemo(t: T) = new Memo[K, V] {
      var tt = t
      override def get(f: K => V)(k: => K) = {
        val kk = k
        m.get(tt)(kk) match {
          case Some(v) => v
          case None => {
            val x = f(kk)
            tt = m.insertKeyValue(tt)(kk)(x)
            x
          }
        }
      }
    }
  }
}
