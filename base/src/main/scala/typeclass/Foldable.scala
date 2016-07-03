package scalaz
package typeclass

trait Foldable[F[_]] {
  def foldLeft[A, B](fa: F[A], z: B)(f: (B, A) => B): B
  def foldRight[A, B](fa: F[A], z: => B)(f: (A, => B) => B): B
  def toList[A](fa: F[A]): List[A]
}

object Foldable extends FoldableInstances {
  def apply[F[_]](implicit F: Foldable[F]): Foldable[F] = F

  object syntax extends FoldableSyntax
}
