package scalaz
package typeclass

trait CategoryClass[=>:[_,_]] extends Category[=>:] with Compose[=>:] {
  def compose: Compose[=>:] with this.type = this
  def id[A]: A =>: A
}
