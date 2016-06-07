package scalaz
package typeclass

trait Semigroup[A] {
  def append(a1: A, a2: => A): A
}

object Semigroup {
  def apply[A](implicit A: Semigroup[A]): Semigroup[A] = A

  object syntax extends SemigroupSyntax
}