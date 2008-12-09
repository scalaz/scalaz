// Copyright Tony Morris 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz

/**
 * An ordering of 'less than', 'equal to' or 'greater than'.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
sealed trait Ordering {
  /**
   * An integer representation of this ordering.
   */
  val toInt: Int
}

/**
 * An ordering of 'less than'.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
final case object LT extends Ordering {
  /**
   * <code>-1</code>.
   */
  val toInt = -1
}

/**
 * An ordering of 'equal to'.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
final case object EQ extends Ordering {
  /**
   * <code>0</code>.
   */
  val toInt = 0
}

/**
 * An ordering of 'greater than'.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
final case object GT extends Ordering {
  /**
   * <code>1</code>.
   */
  val toInt = 1
}

import control.Monoid

/**
 * Functions over ordering.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object Ordering {
  /**
   * Constructs an ordering from the given integer value
   * <pre>
   * n < 0 => LT
   * n == 0 => EQ
   * n > 0 => GT  
   * </pre>
   */
  implicit def fromInt(n: Int) =
    if(n < 0) LT
    else if(n == 0) EQ
    else GT

  /**
   * A monoid for ordering where <code>EQ</code> is identity. 
   */
  implicit val OrderingMonoid = new Monoid[Ordering] {
    val zero = EQ
    def append(x: => Ordering, y: => Ordering) = if(x == EQ) y else x
  }
}
