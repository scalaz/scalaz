package scalaz
package typeclass

trait Category[=>:[_,_]] {
  def compose: Compose[=>:]
  def id[A]: A =>: A
}

object Category {
  def apply[=>:[_, _]](implicit F: Category[=>:]): Category[=>:] = F
}
