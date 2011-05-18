package scalaz

/**
 * Given a type A that has an instance of the Semigroup type class, the type Dual[A] has a Semigroup instance
 * that reverses the arguments to {@link scalaz.Semigroup#append}.
 * <p/>
 * This is only of use when Semigroup[A] is not commutative.
 * <p/>
 * For example:
 * <pre>
 *   ("1" ⊹ "2") ≟ "12"
 *   ("1".σ ⊹ "2".σ) ≟ "21".σ
 * </pre>
 * @see <a href="http://www.haskell.org/ghc/docs/6.10-latest/html/libraries/base/Data-Monoid.html#t%3ADual">Data-Monoid</a>
 * @see scalaz.Semigroup#DualSemigroup
 * @see scalaz.Identity#σ
 */
trait Dual[A] extends NewType[A]

trait Duals {
  implicit def DualTo[A](a: A): Dual[A] = new Dual[A] {
    val value = a
  }
}
