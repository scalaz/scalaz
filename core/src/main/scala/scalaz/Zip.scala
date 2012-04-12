package scalaz

trait Zip[F[_]] { self =>
  def zip[A, B](a: => F[A], b: => F[B]): F[(A, B)]

  def zipWith[A, B, C](fa: => F[A], fb: => F[B])(f: (A, B) => C)(implicit F: Functor[F]): F[C] =
    F.map(zip(fa, fb)) {
      case (a, b) => f(a, b)
    }

  def ap(implicit F: Functor[F]): Apply[F] =
    new Apply[F] {
      def ap[A, B](fa: => F[A])(f: => F[A => B]) =
        zipWith(fa, f)((a, g) => g(a))
      def map[A, B](fa: F[A])(f: A => B) =
        F.map(fa)(f)
    }
}

object Zip {
  @inline def apply[F[_]](implicit F: Zip[F]): Zip[F] = F
}
