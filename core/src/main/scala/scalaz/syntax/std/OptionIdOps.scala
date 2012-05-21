package scalaz.syntax
package std

trait OptionIdOps[A] extends Ops[A] {
  def some: Option[A] = Some(self)
}

trait ToOptionIdOps {
  implicit def ToOptionIdOps[A](a: A) = new OptionIdOps[A] { def self = a }
}
