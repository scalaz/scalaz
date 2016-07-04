package scalaz
package typeclass

trait FoldableClass[F[_]] extends Foldable[F]{
  final def foldable: Foldable[F] = this
}

object FoldableClass {
  trait Template[F[_]] extends FoldableClass[F] with Foldable.FoldRight[F]

  trait AltTemplate[F[_]] extends FoldableClass[F] with Foldable.FoldMap[F]

}