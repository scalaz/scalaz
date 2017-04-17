package scalaz.syntax
package std

final class OptionIdOps[A](val self: A) extends AnyVal {
  def some: Option[A] = Some(self)
}

trait ToOptionIdOps {
  implicit def ToOptionIdOps[A](a: A): OptionIdOps[A] = new OptionIdOps(a)
}
