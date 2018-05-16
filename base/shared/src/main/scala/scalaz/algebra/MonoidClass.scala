package scalaz
package algebra

trait MonoidClass[A] extends SemigroupClass[A] {
  def empty: A
}
