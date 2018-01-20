package scalaz
package typeclass

trait Category[=>:[_,_]] {
  def compose: Compose[=>:] with this.type
  def id[A]: A =>: A
}
