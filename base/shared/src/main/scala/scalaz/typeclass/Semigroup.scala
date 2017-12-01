package scalaz
package typeclass

/**
 * A Semigroup is a Magma with associativity
 */
trait Semigroup[A] {
  def magma:Magma[A]
}

object Semigroup extends SemigroupSyntax {
  def apply[A](implicit A: Semigroup[A]): Semigroup[A] = A
}
