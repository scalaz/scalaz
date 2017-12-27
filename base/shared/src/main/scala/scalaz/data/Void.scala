package scalaz
package data

import scalaz.Prelude._

object Void {
  trait Tag extends Any
  type Type <: Nothing with Tag

  def isNotAny: Void =!= Any = ev => ev.flip.apply(())

  private[this] final class ForcedVoid extends Exception
  def unsafeForce: Type = throw new ForcedVoid
}
