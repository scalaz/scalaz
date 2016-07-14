package scalaz
package typeclass

trait FoldableClass[F[_]] extends Foldable[F]{
  final def foldable: Foldable[F] = this
}