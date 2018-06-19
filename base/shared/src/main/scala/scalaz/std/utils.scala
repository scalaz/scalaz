package scalaz
package std

import kernel.instanceOf
import core.EqClass
import data.Cord
import debug.DebugClass

private[std] object utils {
  def singletonEq[A]: Eq[A]      = instanceOf[EqClass[A]]((a, b) => true)
  def universalEq[A]: Eq[A]      = instanceOf[EqClass[A]]((a, b) => a == b)
  def toStringDebug[A]: Debug[A] = DebugClass.instance[A](a => Cord(a.toString))
}
