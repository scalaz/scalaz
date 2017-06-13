package scalaz
package typeclass

trait MonoidClass[A] extends Monoid[A] with SemigroupClass[A]{
  final def monoid: Monoid[A] = this
}
