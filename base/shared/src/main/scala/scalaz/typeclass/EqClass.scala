package scalaz
package typeclass

trait EqClass[A] {
  def equal(first: A, second: A): Boolean
}
