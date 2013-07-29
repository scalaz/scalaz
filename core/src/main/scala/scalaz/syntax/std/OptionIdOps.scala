package scalaz
package syntax
package std

final class OptionIdOps[A](val self: A) extends Super {
  def some: Option[A] = Some(self)
}

trait ToOptionIdOps {
  implicit def ToOptionIdOps[A](a: A) = new OptionIdOps(a)
}
