package scalaz
package data

trait VoidFunctions {
  def unsafeVoid: Void = throw new Void.Absurd
}

object Void extends VoidFunctions {
  private[data] trait Tag
  type Void <: Nothing with Tag

  private[data] final class Absurd extends RuntimeException
}
