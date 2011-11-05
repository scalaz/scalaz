package scalaz

trait Monoid[F] extends Semigroup[F] { self =>
  ////
  def zero: F

  // derived functions

  ////
  val monoidSyntax = new scalaz.syntax.MonoidSyntax[F] {}
}

object Monoid {
  def apply[F](implicit F: Monoid[F]): Monoid[F] = F

  ////
  import annotation.tailrec

  /**A monoid for sequencing Applicative effects. */
  def liftMonoid[F[_], M](implicit F: Applicative[F], M: Monoid[M]): Monoid[F[M]] = new Monoid[F[M]] {
    val zero = F.point(M.zero)
    def append(x: F[M], y: => F[M]): F[M] = F.lift2[M, M, M]((m1, m2) => M.append(m1, m2))(x, y)
  }

  def unfold[F[_], A, B](seed: A)(f: A => Option[(B, A)])(implicit F: Pointed[F], FB: Monoid[F[B]]): F[B] =
    f(seed) match {
      case None         => FB.zero
      case Some((b, a)) => FB.append(F.point(b), unfold[F, A, B](a)(f))
    }

  def replicate[F[_], A](a: A)(n: Int, f: A => A = (a: A) => a)(implicit P: Pointed[F], FA: Monoid[F[A]]): F[A] = {
    @tailrec
    def replicate0(accum: F[A], n: Int, a: A): F[A] =
      if (n > 0) replicate0(FA.append(accum, P.point(a)), n - 1, f(a)) else accum

    replicate0(FA.zero, n, a)
  }

  ////
}

