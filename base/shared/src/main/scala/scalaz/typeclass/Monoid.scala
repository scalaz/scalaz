package scalaz
package typeclass

trait Monoid[A] {
  def semigroup: Semigroup[A]
  def empty: A
}
