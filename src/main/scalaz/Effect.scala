// Copyright Tony Morris 2008-2009
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz

/**
 * A wrapper around a side-effect (<code>scala.Unit</code>)
 *
 * @see scala.Unit
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait Effect {
  /**
   * The value of this side-effect.
   */
  def effect: Unit

  /**
   * Executes this side-effect if the given argument is <code>false</code>.
   */
  def unless(c: Boolean) = if(!c) effect

  /**
   * Executes this side-effect if the given argument is <code>true</code>.
   */
  def when(c: Boolean) = if(c) effect
}

/**
 * Functions over side-effects.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Effect {
  /**
   * Wraps a <code>scala.Unit</code>.
   */
  implicit def UnitEffect(u: => Unit): Effect = new Effect {
    def effect = u
  }

  /**
   * Unwraps a <code>scala.Unit</code>.
   */
  implicit def EffectUnit(e: Effect) = e.effect
}
