package scalaz
package typeclass

trait Monoid[A] {
  def semigroup: Semigroup[A]
  def empty: A
}

object Monoid {
  def apply[A](implicit A: Monoid[A]): Monoid[A] = A
}
