package scalaz
package typeclass

import scalaz.data.Cord

/** A typeclass describing types which can be meaningfully represented as a `String`.
  */
trait Show[A] {
  /** Produce a `String` representation of `a`.
    */
  /* todo: when we have a `Cord`, change this to return that instead,
   * todo: and add a `def shows(a: A): String = show(a).toString`
   */
  def show(a: A): Cord = Cord(shows(a))
  def shows(a: A): String = Cord.fold(show(a))
}

object Show {
  def apply[A](implicit A: Show[A]): Show[A] = A
  /** A factory for `Show` instances which delegates to Scala's `.toString` method.
    */
  def fromToString[A]: Show[A] = new Show[A] {
    override def shows(a: A): String = a.toString
  }
  def fromShows[A](f: A => String): Show[A] = new Show[A] {
    override def shows(a: A): String = f(a)
  }
  def fromShow[A](f: A => Cord): Show[A] = new Show[A] {
    override def show(a: A): Cord = f(a)
  }
}
