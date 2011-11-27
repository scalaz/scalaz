package scalaz

////
/**
 * @see [[scalaz.Semigroup.SemigroupLaw]]
 * @see [[scalaz.syntax.SemigroupV]]
 */
////
trait Semigroup[F]  { self =>
  ////

  def append(f1: F, f2: => F): F

  // derived functions

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
  def firstSemigroup[A] = new Semigroup[A] {
    def append(f1: A, f2: => A): A = f1
  }

  def lastSemigroup[A] = new Semigroup[A] {
    def append(f1: A, f2: => A): A = f2
  }

  def repeat[F[_], A](a: A)(implicit F: Pointed[F], m: Semigroup[F[A]]): F[A] =
    m.append(F.point(a), repeat[F, A](a))

  def iterate[F[_], A](a: A)(f: A => A)(implicit F: Pointed[F], m: Semigroup[F[A]]): F[A] =
    m.append(F.point(a), iterate[F, A](f(a))(f))

  ////
}

