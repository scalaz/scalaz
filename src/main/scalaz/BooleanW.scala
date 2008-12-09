// Copyright Tony Morris 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz

/**
 * Wraps <code>scala.Boolean</code> and provides additional methods.
 *
 * @see scala.Boolean
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait BooleanW {
  /**
   * The value of this boolean.
   */
  val isTrue: Boolean

  /**
   * Negation of Conjunction.
   *
   * <pre>
   * p q  p ~&& q
   * 0 0  1
   * 0 1  1
   * 1 0  1
   * 1 1  0
   * </pre>
   */
  def ~&&(q: => BooleanW) = !isTrue || !q.isTrue

  /**
   * Negation of Disjunction.
   * 
   * <pre>
   * p q  p ~|| q
   * 0 0  1
   * 0 1  0
   * 1 0  0
   * 1 1  0
   * </pre>
   */
  def ~||(q: => BooleanW) = !isTrue && !q.isTrue

  /**
   * Conditional.
   *
   * <pre>
   * p q  p ==> q
   * 0 0  1
   * 0 1  1
   * 1 0  0
   * 1 1  1
   * </pre>
   */
  def ==>(q: => BooleanW) = !isTrue || q.isTrue

  /**
   * Inverse Conditional.
   *
   * <pre>
   * p q  p <== q
   * 0 0  1
   * 0 1  0
   * 1 0  1
   * 1 1  1
   * </pre>
   */
  def <==(q: => BooleanW) = isTrue || !q.isTrue

  /**
   * Negational of Conditional.
   *
   * <pre>
   * p q  p ~==> q
   * 0 0  0
   * 0 1  0
   * 1 0  1
   * 1 1  0
   * </pre>
   */
  def ~==>(q: => BooleanW) = isTrue && !q.isTrue

  /**
   * Negation of Inverse Conditional.
   *
   * <pre>
   * p q  p ~<== q
   * 0 0  0
   * 0 1  1
   * 1 0  0
   * 1 1  0
   * </pre>
   */
  def ~<==(q: => BooleanW) = !isTrue && q.isTrue

  /**
   * Executes the given side-effect if this boolean value is <code>true</code>.
   *
   * @param t The side-effect to conditionally execute.
   */
  def !>(t: => Unit) = if(isTrue) t

  /**
   * Executes the given side-effect if this boolean value is <code>false</code>.
   *
   * @param t The side-effect to conditionally execute.
   */
  def unless(f: => Unit) = if(!isTrue) f

  /**
   * Conditional operator that returns the first argument if this is <code>true</code>, the second argument otherwise.
   *
   * @param t The value to return if this is <code>true</code>.
   * @param f The value to return if this is <code>false</code>.
   */
  def ?[X](t: => X, f: => X) = if(isTrue) t else f

  /**
   * Returns the given argument in <code>Some</code> if this is <code>true</code>, <code>None</code> otherwise.
   *
   * @param a The argument to conditionally return in <code>Some</code>.
   */
  def toOption[A](a: => A) = if(isTrue) Some(a) else None

  /**
   * Returns the second argument in <code>Left</code> if this is <code>true</code>, otherwise the first argument in
   * <code>Right</code>.
   *
   * @param a The argument to conditionally return in <code>Left</code>.
   * @param b The argument to conditionally return in <code>Right</code>.  
   */
  def toEither[A, B](a: => A, b: => B) = if(isTrue) Right(b) else Left(a)
}

/**
 * Functions over boolean values.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object BooleanW {
  /**
   * Wraps a <code>scala.Boolean</code>.
   */
  implicit def BooleanBooleanW(b: Boolean): BooleanW = new BooleanW {
    val isTrue = b
  }

  /**
   * Unwraps a <code>scala.Boolean</code>.
   */
  implicit def BooleanWBoolean(b: BooleanW) = b.isTrue

  /**
   * An extractor for a boolean wrapper.
   */
  def unapply(b: BooleanW): Option[Boolean] = Some(b.isTrue)
}
