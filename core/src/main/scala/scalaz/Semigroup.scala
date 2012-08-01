package scalaz

////
/**
 * An associative binary operation, circumscribed by type and the
 * semigroup laws.  Unlike [[scalaz.Monoid]], there is not necessarily
 * a zero.
 *
 * @see [[scalaz.Semigroup.SemigroupLaw]]
 * @see [[scalaz.syntax.SemigroupOps]]
 * @see [[http://mathworld.wolfram.com/Semigroup.html]]
 */
////
trait Semigroup[F]  { self =>
  ////
  /**
   * The binary operation to combine `f1` and `f2`.
   *
   * Implementations should not evaluate tbe by-name parameter `f2` if result
   * can be determined by `f1`.
   */
  def append(f1: F, f2: => F): F

  // derived functions
  private[scalaz] trait SemigroupCompose extends Compose[({type λ[α, β]=F})#λ] {
    def compose[A, B, C](f: F, g: F) = append(f, g)
  }

  final def compose: Compose[({type λ[α, β]=F})#λ] = new SemigroupCompose {}

  private[scalaz] trait SemigroupApply extends Apply[({type λ[α]=F})#λ] {
    override def map[A, B](fa: F)(f: (A) => B) = fa
    def ap[A, B](fa: => F)(f: => F) = append(f, fa)
  }

  final def apply: Apply[({type λ[α]=F})#λ] = new SemigroupApply {}

  /**
   * A semigroup in type F must satisfy two laws:
    *
    *  - '''closure''': `∀ a, b in F, append(a, b)` is also in `F`. This is enforced by the type system.
    *  - '''associativity''': `∀ a, b, c` in `F`, the equation `append(append(a, b), c) = append(a, append(b , c))` holds.
   */
  trait SemigroupLaw {
    def associative(f1: F, f2: F, f3: F)(implicit F: Equal[F]): Boolean =
      F.equal(append(f1, append(f2, f3)), append(append(f1, f2), f3))
  }
  def semigroupLaw = new SemigroupLaw {}


  ////
  val semigroupSyntax = new scalaz.syntax.SemigroupSyntax[F] {}
}

object Semigroup {
  @inline def apply[F](implicit F: Semigroup[F]): Semigroup[F] = F

  ////
  /** A purely left-biased semigroup. */
  def firstSemigroup[A] = new Semigroup[A] {
    def append(f1: A, f2: => A): A = f1
  }

  /** A purely right-biased semigroup. */
  def lastSemigroup[A] = new Semigroup[A] {
    def append(f1: A, f2: => A): A = f2
  }

  def minSemigroup[A](implicit o: Order[A]): Semigroup[A] = new Semigroup[A] {
    def append(f1: A, f2: => A): A = o.min(f1, f2)
  }

  def maxSemigroup[A](implicit o: Order[A]): Semigroup[A] = new Semigroup[A] {
    def append(f1: A, f2: => A): A = o.max(f1, f2)
  }

  def repeat[F[_], A](a: A)(implicit F: Pointed[F], m: Semigroup[F[A]]): F[A] =
    m.append(F.point(a), repeat[F, A](a))

  def iterate[F[_], A](a: A)(f: A => A)(implicit F: Pointed[F], m: Semigroup[F[A]]): F[A] =
    m.append(F.point(a), iterate[F, A](f(a))(f))

  ////
}

