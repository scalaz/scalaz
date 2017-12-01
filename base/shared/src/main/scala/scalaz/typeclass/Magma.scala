package scalaz
package typeclass

/* A Magma is a Set with a binary operation that returns a member of the same Set
 * */
trait Magma[A] {
  def append(a1: A, a2: => A): A
}

object Magma extends MagmaInstances {
  def apply[A](implicit A: Magma[A]): Magma[A] = A
}
