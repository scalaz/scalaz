package scalaz
package typeclass

trait FoldableClass[F[_]] extends Foldable[F]{
  implicit final def foldable: Foldable[F] = this
}
