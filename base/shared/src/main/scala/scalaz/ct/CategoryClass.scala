package scalaz
package ct

trait CategoryClass[=>:[_, _]] extends ComposeClass[=>:] {
  def id[A]: A =>: A
}
