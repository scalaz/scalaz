package scalaz
package typeclass

/* A Group is a Monoid with negation and inverse, such that
 * a |+| inverse(a)  = monoid.empty
 * inverse(a) |+| a  = monoid.empty
 *  
 * */
trait Group[A] {
  def monoid: Monoid[A]
  def inverse(a: A): A
}

object Group extends GroupInstances {
  def apply[A](implicit A: Group[A]): Group[A] = A
}
