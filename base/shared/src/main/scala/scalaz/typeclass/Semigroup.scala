package scalaz
package typeclass

trait Semigroup[A] {
  def append(a1: A, a2: => A): A
}
