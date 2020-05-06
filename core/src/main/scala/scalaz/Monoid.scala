package scalaz

////
/**
 * Provides an identity element (`zero`) to the binary `append`
 * operation in [[scalaz.Semigroup]], subject to the monoid laws.
 *
 * Example instances:
 *  - `Monoid[Int]`: `zero` and `append` are `0` and `Int#+` respectively
 *  - `Monoid[List[A]]`: `zero` and `append` are `Nil` and `List#++` respectively
 *
 * References:
 *  - [[http://mathworld.wolfram.com/Monoid.html]]
 *
 * @see [[scalaz.syntax.MonoidOps]]
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
   */
  def multiply(value: F, n: Int): F =
    if (n <= 0) zero else multiply1(value, n - 1)

  /** Whether `a` == `zero`. */
  def isMZero(a: F)(implicit eq: Equal[F]): Boolean =
    eq.equal(a, zero)

  final def ifEmpty[B](a: F)(t: => B)(f: => B)(implicit eq: Equal[F]): B =
    if (isMZero(a)) { t } else { f }

  final def onNotEmpty[B](a: F)(v: => B)(implicit eq: Equal[F], mb: Monoid[B]): B =
    ifEmpty(a)(mb.zero)(v)

  final def onEmpty[A,B](a: F)(v: => B)(implicit eq: Equal[F], mb: Monoid[B]): B =
    ifEmpty(a)(v)(mb.zero)

  /** Every `Monoid` gives rise to a [[scalaz.Category]], for which
    * the type parameters are phantoms.
    *
    * @note `category.monoid` = `this`
    */
  final def category: Category[λ[(α, β) => F]] =
    new Category[λ[(α, β) => F]] with SemigroupCompose {
      def id[A] = zero
    }

  /**
   * A monoidal applicative functor, that implements `point` and `ap`
   * with the operations `zero` and `append` respectively.  Note that
   * the type parameter `α` in `Applicative[λ[α => F]]` is
   * discarded; it is a phantom type.  As such, the functor cannot
   * support [[scalaz.Bind]].
   */
  final def applicative: Applicative[λ[α =>F]] =
    new Applicative[λ[α => F]] with SemigroupApply {
      def point[A](a: => A) = zero
    }

  /**
   * Monoid instances must satisfy [[scalaz.Semigroup.SemigroupLaw]] and 2 additional laws:
   *
   *  - '''left identity''': `forall a. append(zero, a) == a`
   *  - '''right identity''' : `forall a. append(a, zero) == a`
   */
  trait MonoidLaw extends SemigroupLaw {
    def leftIdentity(a: F)(implicit F: Equal[F]): Boolean = F.equal(a, append(zero, a))
    def rightIdentity(a: F)(implicit F: Equal[F]): Boolean = F.equal(a, append(a, zero))
  }
  def monoidLaw = new MonoidLaw {}

  ////
  val monoidSyntax: scalaz.syntax.MonoidSyntax[F] =
    new scalaz.syntax.MonoidSyntax[F] { def F = Monoid.this }
}

object Monoid {
  @inline def apply[F](implicit F: Monoid[F]): Monoid[F] = F

  import Isomorphism._

  def fromIso[F, G](D: F <=> G)(implicit M: Monoid[G]): Monoid[F] =
    new IsomorphismMonoid[F, G] {
      override def G: Monoid[G] = M
      override def iso: F <=> G = D
    }

  ////

  /** Make an append and zero into an instance. */
  def instance[A](f: (A, => A) => A, z: A): Monoid[A] =
    new Monoid[A] {
      def zero = z
      def append(f1: A, f2: => A): A = f(f1,f2)
    }

  private trait ApplicativeMonoid[F[_], M] extends Monoid[F[M]] with Semigroup.ApplySemigroup[F, M] {
    implicit def F: Applicative[F]
    implicit def M: Monoid[M]
    val zero = F.point(M.zero)
  }

  /**A monoid for sequencing Applicative effects. */
  def liftMonoid[F[_], M](implicit F0: Applicative[F], M0: Monoid[M]): Monoid[F[M]] =
    new ApplicativeMonoid[F, M] {
      implicit def F: Applicative[F] = F0
      implicit def M: Monoid[M] = M0
    }

  def liftPlusEmpty[A](implicit M0: Monoid[A]): PlusEmpty[λ[α => A]] =
    new PlusEmpty[λ[α => A]] {
      type A0[α] = A
      def empty[A]: A0[A] = M0.zero
      def plus[A](f1: A0[A], f2: => A0[A]): A0[A] = M0.append(f1, f2)
    }

  /** Monoid is an invariant functor. */
  implicit val monoidInvariantFunctor: InvariantFunctor[Monoid] =
    new InvariantFunctor[Monoid] {
      def xmap[A, B](ma: Monoid[A], f: A => B, g: B => A): Monoid[B] = new Monoid[B] {
        def zero: B = f(ma.zero)
        def append(x: B, y: => B): B = f(ma.append(g(x), g(y)))
      }
    }

  ////
}
