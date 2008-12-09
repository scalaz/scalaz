// Copyright Tony Morris 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz

/**
 * A type-constructor for a lifted value.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait Proxy[A]

/**
 * Functions over proxies.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Proxy {
  /**
   * Returns a new proxy. 
   */
  def proxy[A] = new Proxy[A]{}
}
