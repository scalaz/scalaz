package scalaz
package typeclass

trait SemigroupClass[A] extends Semigroup[A] with MagmaClass[A]{
  final def semigroup: Semigroup[A] = this
}
