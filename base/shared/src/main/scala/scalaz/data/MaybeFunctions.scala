package scalaz
package data

trait MaybeFunctions {
  def empty[A] = Maybe.empty[A]
  def just[A](a: A) = Maybe.just(a)
}
