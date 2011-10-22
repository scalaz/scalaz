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

  /**A monoid for sequencing Applicative effects. */
  def liftMonoid[F[_], M](implicit F: Applicative[F], M: Monoid[M]): Monoid[F[M]] = new Monoid[F[M]] {
    val zero = F.pure(M.zero)
    def append(x: F[M], y: => F[M]): F[M] = F.lift2[M, M, M]((m1, m2) => M.append(m1, m2))(x, y)
  }

  ////
}

