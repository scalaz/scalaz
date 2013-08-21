package scalaz.syntax
package std

final class OptionIdOps[A](self: A) {
  def some: Option[A] = Some(self)
}

trait ToOptionIdOps {
  implicit def ToOptionIdOps[A](a: A) = new OptionIdOps(a)
}
