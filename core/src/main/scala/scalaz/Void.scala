package scalaz

import Liskov.<~<
import scalaz.syntax.VoidSyntax

trait VoidModule {
  type Void

  def absurd[A](v: Void): A

  def unsafeVoid: Void

  def isNothing: Void === Nothing

  def conforms[A]: Void <~< A
}

object VoidImpl extends VoidModule with VoidSyntax {
  type Void = Nothing

  def absurd[A](v: Void): A = v

  final class UnsafeVoid extends RuntimeException

  def unsafeVoid: Void = throw new UnsafeVoid

  def isNothing: Void === Nothing = Leibniz.refl[Void]

  def conforms[A]: Void <~< A = Liskov.refl[Void]
}
