// Copyright Workingmouse Pty. Ltd. 2007, 2008
// This software is released under an open source BSD licence.

// $LastChangedRevision$
// $LastChangedDate$


package scalaz.lazytuple

import LT2.lt2
import LT3.lt3
import LT4.lt4
import LT5.lt5
import LT6.lt6
import LT7.lt7

/**
 * Functions over lazy tuples.
 *
 * @author <a href="mailto:research@workingmouse.com">Tony Morris</a>
 * @version $LastChangedRevision$<br>
 *          $LastChangedDate$<br>
 *          $LastChangedBy$
 */
object LT {
  /**
   * Construct a lazy tuple-2.
   */
  def apply[A, B](a: => A, b: => B) = lt2(a, b)

  /**
   * Construct a lazy tuple-3.
   */
  def apply[A, B, C](a: => A, b: => B, c: => C) = lt3(a, b, c)

  /**
   * Construct a lazy tuple-4.
   */
  def apply[A, B, C, D](a: => A, b: => B, c: => C, d: => D) = lt4(a, b, c, d)

  /**
   * Construct a lazy tuple-5.
   */
  def apply[A, B, C, D, E](a: => A, b: => B, c: => C, d: => D, e: => E) = lt5(a, b, c, d, e)

  /**
   * Construct a lazy tuple-6.
   */
  def apply[A, B, C, D, E, F](a: => A, b: => B, c: => C, d: => D, e: => E, f: => F) = lt6(a, b, c, d, e, f)

  /**
   * Construct a lazy tuple-7.
   */
  def apply[A, B, C, D, E, F, G](a: => A, b: => B, c: => C, d: => D, e: => E, f: => F, g: => G) = lt7(a, b, c, d, e, f, g)
}
