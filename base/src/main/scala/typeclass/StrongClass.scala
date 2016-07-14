package scalaz
package typeclass

trait StrongClass[P[_, _]] extends Strong[P] with ProfunctorClass[P] {
  final def strong: Strong[P] = this
}