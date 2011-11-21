package scalaz

////
/**
 * A categorical monoid.
 *
 * All monoid instances must satisfy the [[scalaz.Semigroup]] law and 2 additional laws:
 *
 *  - '''left identity''': `forall a. append(zero, a) == a`
 *  - '''right identity''' : `forall a. append(a, zero) == a`
 *
 * @see [[scalaz.syntax.MonoidV]]
 */
////
trait Monoid[F] extends Semigroup[F] { self =>
  ////
  def zero: F

  // derived functions

  trait MonoidLaw extends SemigroupLaw {
    def identity(a: F)(implicit F: Equal[F]) = F.equal(a, append(a, zero)) && F.equal(a, append(zero, a))
  }
  def monoidLaw = new MonoidLaw {}

  ////
  val monoidSyntax = new scalaz.syntax.MonoidSyntax[F] {}
}

object Monoid {
  @inline def apply[F](implicit F: Monoid[F]): Monoid[F] = F

  ////
  import annotation.tailrec

  trait ApplicativeSemigroup[F[_], M] extends Semigroup[F[M]] {
    implicit def F: Applicative[F]
    implicit def M: Semigroup[M]
    def append(x: F[M], y: => F[M]): F[M] = F.lift2[M, M, M]((m1, m2) => M.append(m1, m2))(x, y)
  }

  trait ApplicativeMonoid[F[_], M] extends Monoid[F[M]] with ApplicativeSemigroup[F, M] {
    implicit def M: Monoid[M]
    val zero = F.point(M.zero)
  }

  /**A semigroup for sequencing Applicative effects. */
  def liftSemigroup[F[_], M](implicit F0: Applicative[F], M0: Semigroup[M]): Semigroup[F[M]] = new ApplicativeSemigroup[F, M] {
    implicit def F: Applicative[F] = F0
    implicit def M: Semigroup[M] = M0
  }

  /**A semigroup for sequencing Applicative effects. */
  def liftMonoid[F[_], M](implicit F0: Applicative[F], M0: Monoid[M]): Monoid[F[M]] = new ApplicativeMonoid[F, M] {
    implicit def F: Applicative[F] = F0
    implicit def M: Monoid[M] = M0
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

