package scalaz
package algebra

trait SemigroupClass[A] {
  def append(a1: A, a2: => A): A
}
