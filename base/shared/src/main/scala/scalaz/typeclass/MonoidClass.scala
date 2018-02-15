package scalaz
package typeclass

trait MonoidClass[A] extends SemigroupClass[A] {
  def empty: A
}
