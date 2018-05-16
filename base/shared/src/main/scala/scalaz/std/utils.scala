package scalaz
package std

import typeclass.{ DebugClass, EqClass }

private[std] object utils {
  def singletonEq[A]: Eq[A]      = instanceOf[EqClass[A]]((a, b) => true)
  def universalEq[A]: Eq[A]      = instanceOf[EqClass[A]]((a, b) => a == b)
  def toStringDebug[A]: Debug[A] = instanceOf[DebugClass[A]](a => a.toString)
}
