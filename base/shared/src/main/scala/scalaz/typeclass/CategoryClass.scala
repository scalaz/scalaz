package scalaz
package typeclass

trait CategoryClass[=>:[_,_]] extends Category[=>:] with Compose[=>:] {
  def compose: Compose[=>:] = this
  def id[A]: A =>: A
}
