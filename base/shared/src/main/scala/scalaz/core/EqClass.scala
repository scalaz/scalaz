package scalaz
package core

trait EqClass[A] {
  def equal(first: A, second: A): Boolean
}
