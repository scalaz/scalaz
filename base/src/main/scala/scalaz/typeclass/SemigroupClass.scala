package scalaz
package typeclass

trait SemigroupClass[A] extends Semigroup[A]{
  final def semigroup: Semigroup[A] = this
}
