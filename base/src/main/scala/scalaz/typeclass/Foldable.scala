package scalaz
package typeclass

trait Foldable[F[_]] {

  def foldMap[A, B: Monoid](fa: F[A])(f: A => B): B

  def foldRight[A, B](fa: F[A], z: => B)(f: (A, => B) => B): B

  def foldLeft[A, B](fa: F[A], z: B)(f: (B, A) => B): B // = TODO default implementation from foldmap

  // TODO Use IList (`toIList`)
  def toList[A](fa: F[A]): List[A] = foldLeft(fa, List[A]())((t, h) => h :: t).reverse
}

object Foldable extends FoldableInstances with FoldableSyntax {
  def apply[F[_]](implicit F: Foldable[F]): Foldable[F] = F
}
