package scalaz
package ct

trait ComposeClass[=>:[_, _]] {
  def compose[A, B, C](f: B =>: C, g: A =>: B): (A =>: C)
}
