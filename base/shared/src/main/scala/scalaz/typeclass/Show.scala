package scalaz
package typeclass

/** A typeclass describing types which can be meaningfully represented as a `String`.
  */
trait Show[A] {
  /** Produce a `String` representation of `a`.
    */
  /* todo: when we have a `Cord`, change this to return that instead,
   * todo: and add a `def shows(a: A): String = show(a).toString`
   */
  def show(a: A): String
}

object Show {
  /** A factory for `Show` instances which delegates to Scala's `.toString` method.
    */
  def fromToString[A]: Show[A] = _.toString()
}
