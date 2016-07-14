package scalaz
package typeclass

trait TraversableClass[T[_]] extends Traversable[T] with FunctorClass[T] with FoldableClass[T] {
  final def traversable: Traversable[T] = this
}