package scalaz

////
/**
 * Provides an identity element (`zero`) to the binary `append` operation in [[scalaz.Semigroup]].
 *
 * Example instances:
 *  - `Monoid[Int]`: `zero` and `append` are `0` and `Int#+` respectively
 *  - `Monoid[List[A]]`: `zero` and `append` are `Nil` and `List#++` respectively
 *
 * References:
 *  - [[http://mathworld.wolfram.com/Monoid.html]]
 *
 * @see [[scalaz.syntax.MonoidV]]
 * @see [[scalaz.Monoid.MonoidLaw]]
 *
 */
////
trait Monoid[F] extends Semigroup[F] { self =>
  ////
  /** The identity element for `append`. */
  def zero: F

  // derived functions
  /**
   * For `n = 0`, `zero`
   * For `n = 1`, `append(zero, value)`
   * For `n = 2`, `append(append(zero, value), value)`
   *
   * [[scalaz.Monoid]]`.replicate` generalizes this function.
   */
  final def multiply(value: F, n: Int): F = Monoid.replicate[Id, F](value)(n)(implicitly, this)

  /** Every `Monoid` gives rise to a [[scalaz.Category]] */
  final def category: Category[({type λ[α, β]=F})#λ] = new Category[({type λ[α, β]=F})#λ] with SemigroupCompose {
    def id[A] = zero
  }

  /**
   * A monoidal applicative functor, that implements `point` and `ap` with the operations `zero` and `append` respectively.
   * Note that the type parameter `α` in `Applicative[({type λ[α]=F})#λ]` is discarded; it is a phantom type.
   */
  final def applicative: Applicative[({type λ[α]=F})#λ] = new Applicative[({type λ[α]=F})#λ] with SemigroupApply {
    def point[A](a: => A) = zero
  }

  /**
   * Monoid instances must satisfy [[scalaz.Semigroup.SemigroupLaw]] and 2 additional laws:
   *
   *  - '''left identity''': `forall a. append(zero, a) == a`
   *  - '''right identity''' : `forall a. append(a, zero) == a`
   */
  trait MonoidLaw extends SemigroupLaw {
    def leftIdentity(a: F)(implicit F: Equal[F]) = F.equal(a, append(zero, a))
    def rightIdentity(a: F)(implicit F: Equal[F]) = F.equal(a, append(a, zero))
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

