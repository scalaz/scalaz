package scalaz
package typeclass

trait SemigroupClass[A] {
  def append(a1: A, a2: => A): A
}