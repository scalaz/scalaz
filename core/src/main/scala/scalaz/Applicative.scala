package scalaz

trait Applicative[F[_]] extends Apply[F] with Pointed[F] { self =>
  ////

  // derived functions
  def map[A,B](fa: F[A])(f: A => B): F[B] =
    ap(fa)(pure(f))
  override def map2[A,B,C](fa: F[A], fb: F[B])(f: (A,B) => C): F[C] =
    ap2(fa, fb)(pure(f))

  // impls of sequence, traverse, etc

  ////
  val applicativeSyntax = new scalaz.syntax.ApplicativeSyntax[F] {}
}

object Applicative {
  def apply[F[_]](implicit F: Applicative[F]): Applicative[F] = F

  ////

  ////
}

