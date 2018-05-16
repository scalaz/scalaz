package scalaz
package std

import typeclass.{ DebugClass, EqClass }

object utils {
  private[std] def singletonEq[A]: Eq[A]      = instanceOf[EqClass[A]]((a, b) => true)
  private[std] def universalEq[A]: Eq[A]      = instanceOf[EqClass[A]]((a, b) => a == b)
  private[std] def toStringDebug[A]: Debug[A] = instanceOf[DebugClass[A]](a => a.toString)
}
