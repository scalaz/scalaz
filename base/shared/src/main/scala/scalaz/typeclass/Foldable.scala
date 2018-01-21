package scalaz
package typeclass

sealed trait Foldable[F[_]] extends Foldable.Class[F]

object Foldable {

  trait Class[F[_]] {

    def foldMap[A, B: Monoid](fa: F[A])(f: A => B): B

    def foldRight[A, B](fa: F[A], z: => B)(f: (A, => B) => B): B

    def foldLeft[A, B](fa: F[A], z: B)(f: (B, A) => B): B // = TODO default implementation from foldmap

    // TODO Use IList (`toIList`)
    def toList[A](fa: F[A]): List[A]

    def foldable: Foldable[F]
  }

  trait Template[F[_]] extends Foldable[F] {
    final override def foldable = this
  }

  trait DeriveToList[F[_]] { self: Class[F] =>
    final override def toList[A](fa: F[A]) = foldLeft(fa, List[A]())((t, h) => h :: t).reverse
  }

  trait Alt[D <: Alt[D]]
  trait DeriveFoldRight[F[_]] extends Alt[DeriveFoldRight[F]] { self : Class[F] =>
    override def foldRight[A, B](fa: F[A], z: => B)(f: (A, => B) => B): B  // = TODO implement from foldmap/endo
  }
  trait DeriveFoldMap[F[_]] extends Alt[DeriveFoldMap[F]] { self : Class[F] =>
    final override def foldMap[A, B](fa: F[A])(f: A => B)(implicit B: Monoid[B]) = foldRight(fa, B.empty)((a, b) => B.append(f(a),b))
  }

  def apply[F[_]](implicit F: Foldable[F]): Foldable[F] = F
}