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

  /** Every `Semigroup` gives rise to a [[scalaz.Compose]], for which
    * the type parameters are phantoms.
    *
    * @note `compose.semigroup` = `this`
    */
  final def compose: Compose[({type λ[α, β]=F})#λ] = new SemigroupCompose {}

  private[scalaz] trait SemigroupApply extends Apply[({type λ[α]=F})#λ] {
    override def map[A, B](fa: F)(f: A => B) = fa
    def ap[A, B](fa: => F)(f: => F) = append(f, fa)
  }

  /**
   * An [[scalaz.Apply]], that implements `ap` with `append`.  Note
   * that the type parameter `α` in `Apply[({type λ[α]=F})#λ]` is
   * discarded; it is a phantom type.  As such, the functor cannot
   * support [[scalaz.Bind]].
   */
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
  val semigroupSyntax = new scalaz.syntax.SemigroupSyntax[F] { def F = Semigroup.this }
}

object Semigroup {
  @inline def apply[F](implicit F: Semigroup[F]): Semigroup[F] = F

  ////
  /** Make an associative binary function into an instance. */
  def instance[A](f: (A, => A) => A): Semigroup[A] = new Semigroup[A] {
    def append(f1: A, f2: => A): A = f(f1,f2)
  }

  /** A purely left-biased semigroup. */
  def firstSemigroup[A] = new Semigroup[A] {
    def append(f1: A, f2: => A): A = f1
  }

  @inline implicit def firstTaggedSemigroup[A] = firstSemigroup[A @@ Tags.FirstVal]

  /** A purely right-biased semigroup. */
  def lastSemigroup[A] = new Semigroup[A] {
    def append(f1: A, f2: => A): A = f2
  }

  @inline implicit def lastTaggedSemigroup[A] = lastSemigroup[A @@ Tags.LastVal]

  def minSemigroup[A](implicit o: Order[A]): Semigroup[A @@ Tags.MinVal] = new Semigroup[A @@ Tags.MinVal] {
    def append(f1: A @@ Tags.MinVal, f2: => A @@ Tags.MinVal) = Tags.MinVal(o.min(f1, f2))
  }

  @inline implicit def minTaggedSemigroup[A : Order] = minSemigroup[A]

  def maxSemigroup[A](implicit o: Order[A]): Semigroup[A @@ Tags.MaxVal] = new Semigroup[A @@ Tags.MaxVal] {
    def append(f1: A @@ Tags.MaxVal, f2: => A @@ Tags.MaxVal) = Tags.MaxVal(o.max(f1, f2))
  }

  @inline implicit def maxTaggedSemigroup[A : Order] = maxSemigroup[A]

  private[scalaz] trait ApplySemigroup[F[_], M] extends Semigroup[F[M]] {
    implicit def F: Apply[F]
    implicit def M: Semigroup[M]
    def append(x: F[M], y: => F[M]): F[M] = F.lift2[M, M, M]((m1, m2) => M.append(m1, m2))(x, y)
  }

  /**A semigroup for sequencing Apply effects. */
  def liftSemigroup[F[_], M](implicit F0: Apply[F], M0: Semigroup[M]): Semigroup[F[M]] = new ApplySemigroup[F, M] {
    implicit def F: Apply[F] = F0
    implicit def M: Semigroup[M] = M0
  }

  /** `point(a) append (point(a) append (point(a)...` */
  def repeat[F[_], A](a: A)(implicit F: Applicative[F], m: Semigroup[F[A]]): F[A] =
    m.append(F.point(a), repeat[F, A](a))

  /** `point(a) append (point(f(a)) append (point(f(f(a)))...` */
  def iterate[F[_], A](a: A)(f: A => A)(implicit F: Applicative[F], m: Semigroup[F[A]]): F[A] =
    m.append(F.point(a), iterate[F, A](f(a))(f))

  ////
}
