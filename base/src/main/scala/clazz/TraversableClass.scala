package scalaz
package clazz

trait TraversableClass[F[_]] extends Traversable[F] with FunctorClass[F] with FoldableClass[F] {
  implicit final def traversable: Traversable[F] = this
}
