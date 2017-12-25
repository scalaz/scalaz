package scalaz
package typeclass

/**
 * A Semigroup is a binary opertion such that:
 * 1 - The result is in the same Set/Type
 * 2 - Is associative
 */
trait Semigroup[A] {
  def append(a1: A, a2: => A): A
}

object Semigroup extends SemigroupSyntax {
  def apply[A](implicit A: Semigroup[A]): Semigroup[A] = A
}
