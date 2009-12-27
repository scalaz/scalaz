package scalaz

/**
 * @see <a href="http://www.haskell.org/ghc/docs/6.10-latest/html/libraries/base/Data-Monoid.html#t%3ADual">Data-Monoid</a>
 * @see Semigroup.DualSemigroup
 */
trait Dual[A] extends NewType[A]

trait Duals {
  implicit def DualTo[A](a: A): Dual[A] = new Dual[A] {
    val value = a
  }
}
