package scalaz
package typeclass

trait Compose[=>:[_, _]] {
  def compose[A, B, C](f: B =>: C, g: A =>: B): (A =>: C)
}
