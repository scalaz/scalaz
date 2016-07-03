package scalaz
package typeclass

trait FoldableClass[F[_]] extends Foldable[F]{
  final def foldable: Foldable[F] = this
}

object FoldableClass {
  trait Template[F[_]] extends FoldableClass[F]

  trait ToList[F[_]] { self: FoldableClass[F] =>
    override def toList[A](fa: F[A]): List[A] = foldLeft(fa, List[A]())((t, h) => h :: t).reverse
  }
}