package scalaz
package typeclass

trait Monoid[A] {
  def semigroup: Semigroup[A] with this.type
  def empty: A
}
