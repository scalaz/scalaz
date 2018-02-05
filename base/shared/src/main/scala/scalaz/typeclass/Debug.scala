package scalaz
package typeclass

import scalaz.data.Cord

/** A typeclass describing types which can be meaningfully represented as a `String`, for debugging purposes.
  */
trait Debug[A] {
  /** Produce a `String` representation of `a`.
    */
  def debug(a: A): Cord = Cord(debugs(a))
  def debugs(a: A): String = Cord.fold(debug(a))
}

object Debug {
  def apply[A](implicit A: Debug[A]): Debug[A] = A
  /** A factory for `Debug` instances which delegates to Scala's `.toString` method.
    */
  def fromToString[A]: Debug[A] = new Debug[A] {
    override def debugs(a: A): String = a.toString
  }
  def fromDebugs[A](f: A => String): Debug[A] = new Debug[A] {
    override def debugs(a: A): String = f(a)
  }
  def fromDebug[A](f: A => Cord): Debug[A] = new Debug[A] {
    override def debug(a: A): Cord = f(a)
  }
}