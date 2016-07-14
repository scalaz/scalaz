package scalaz
package typeclass

trait Foldable[F[_]] {

  def foldMap[A, B: Monoid](fa: F[A])(f: A => B): B

  def foldRight[A, B](fa: F[A], z: => B)(f: (A, => B) => B): B

  def foldLeft[A, B](fa: F[A], z: B)(f: (B, A) => B): B // = TODO implementation from foldmap

  def toList[A](fa: F[A]): List[A] = foldLeft(fa, List[A]())((t, h) => h :: t).reverse
}

object Foldable extends FoldableInstances {

  trait FoldMap[F[_]] extends Alt[FoldMap[F]] { self : Foldable[F] =>
    override def foldMap[A, B: Monoid](fa: F[A])(f: A => B) = foldRight(fa, Monoid[B].empty)((a, b) => Semigroup[B].append(f(a),b))
  }
  trait FoldRight[F[_]] extends Alt[FoldRight[F]] { self : Foldable[F] =>
    override def foldRight[A, B](fa: F[A], z: => B)(f: (A, => B) => B): B  // = TODO implement from foldmap/endo
  }
  trait Alt[D <: Alt[D]] { self: D => }

  def apply[F[_]](implicit F: Foldable[F]): Foldable[F] = F

  object syntax extends FoldableSyntax
}
