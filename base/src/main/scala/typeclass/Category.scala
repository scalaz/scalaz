package scalaz
package typeclass

trait Category[=>:[_,_]] {
  def compose: Compose[=>:]
  def id[A]: A =>: A
}
