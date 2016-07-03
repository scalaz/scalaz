package scalaz
package typeclass

trait TraversableClass[T[_]] extends Traversable[T] with FunctorClass[T] with FoldableClass[T] {
  final def traversable: Traversable[T] = this
}

object TraversableClass {

  trait Template[T[_]] extends TraversableClass[T] with FoldableClass.ToList[T] with Traversable.Sequence[T]

  trait AltTemplate[T[_]] extends TraversableClass[T] with FoldableClass.ToList[T] with Traversable.Traverse[T]
}