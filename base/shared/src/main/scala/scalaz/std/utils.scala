package scalaz
package std

import scala.{ AnyRef, AnyVal }

import core.{ EqAnyRef, EqClass }
import data.Cord
import debug.DebugClass

private[std] object utils {
  def singletonEq[A]: Eq[A] = instanceOf(((a: A, b: A) => true): EqClass[A])

  def universalEqAnyRef[A <: AnyRef]: Eq[A] = instanceOf(((a: A, b: A) => a == b): EqAnyRef[A])
  def universalEqAnyVal[A <: AnyVal]: Eq[A] = instanceOf(((a, b) => a == b): EqClass[A])

  def toStringDebug[A]: Debug[A] = DebugClass.instance[A](a => Cord(a.toString))
}
