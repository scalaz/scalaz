package scalaz.syntax
package std

trait OptionIdV[A] extends SyntaxV[A] {
  def some: Option[A] = Some(self)
}

trait ToOptionIdV {
  implicit def ToOptionIdV[A](a: A) = new OptionIdV[A] { def self = a }
}
