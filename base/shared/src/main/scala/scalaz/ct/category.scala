package scalaz
package ct

trait CategoryClass[=>:[_, _]] extends SemigroupoidClass[=>:] {
  def id[A]: A =>: A
}
