package scalaz
package debug

/** A typeclass describing types which can be meaningfully represented as a `String`.
 */
trait DebugClass[A] {

  /** Produce a `String` representation of `a`.
   */
  /* todo: when we have a `Cord`, change this to return that instead,
   * todo: and add a `def debugs(a: A): String = debug(a).toString`
   */
  def debug(a: A): String
}

object DebugClass {

  /** A factory for `Debug` instances which delegates to Scala's `.toString` method.
   */
  def fromToString[A]: Debug[A] = instanceOf[DebugClass[A]](_.toString)
}
