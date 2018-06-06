import scalaz.meta.minimal

@minimal("deferred")
trait deferred {
  def deferred: Int
}