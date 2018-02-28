package scalaz
package typeclass

trait CategoryClass[=>:[_, _]] extends ComposeClass[=>:] {
  def id[A]: A =>: A
}
