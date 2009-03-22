package scalaz.data

/**
 * A digit from 0 to 9.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait Digit {
  /**
   * An integer value for this digit.
   */
  val toInt: Int

  /**
   * A long value for this digit.
   */
  val toLong = toInt.toLong

  /**
   *   A character value for this digit.
   */
  def toChar = (toLong + 48).toChar

   /**
   * Compare two digits for equality.
   */
  override def equals(o: Any) = o.isInstanceOf[Digit] && o.asInstanceOf[Digit].toInt == toInt

  /**
   * A hash code for this digit.
   */
  override def hashCode = toInt

  /**
   * A String representation of this digit.
   */
  override def toString = toInt.toString
}
/**
 * Zero.
 */
final case object _0 extends Digit {
  /**
   * <code>0</code>.
   */
  override val toInt = 0
}
/**
 * One.
 */
final case object _1 extends Digit {
  /**
   * <code>1</code>.
   */
  override val toInt = 1
}
/**
 * Two.
 */
final case object _2 extends Digit {
  /**
   * <code>2</code>.
   */
  override val toInt = 2
}
/**
 * Three.
 */
final case object _3 extends Digit {
  /**
   * <code>3</code>.
   */
  override val toInt = 3
}
/**
 * Four.
 */
final case object _4 extends Digit {
  /**
   * <code>4</code>.
   */
  override val toInt = 4
}
/**
 * Five.
 */
final case object _5 extends Digit {
  /**
   * <code>5</code>.
   */
  override val toInt = 5
}
/**
 * Six.
 */
final case object _6 extends Digit {
  /**
   * <code>6</code>.
   */
  override val toInt = 6
}
/**
 * Seven.
 */
final case object _7 extends Digit {
  /**
   * <code>7</code>.
   */
  override val toInt = 7
}
/**
 * Eight.
 */
final case object _8 extends Digit {
  /**
   * <code>8</code>.
   */
  override val toInt = 8
}
/**
 * Nine.
 */
final case object _9 extends Digit {
  /**
   * <code>9</code>.
   */
  override val toInt = 9
}


/**
 * Functions over digits.
 *
 * @author <a href="mailto:code@tmorris.net">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Digit {
  /**
   * All digits.
   */
  val digits = List(_0, _1, _2, _3, _4, _5, _6, _7, _8, _9)
}
