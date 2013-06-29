package scalaz
package syntax

/** Wraps a value `self` and provides methods related to [[Optional]].
  *
  * @see [[Optional]]
  */
trait OptionalOps[F[_],A] extends Ops[F[A]] {

  implicit def F: Optional[F]

  /** Returns the value within the context if defined or else the value of `default`. */
  def getOrElse(default: => A): A = F.getOrElse(self)(default)

  /** Returns `true` if a value is defined within the context. */
  def isDefined: Boolean = F.isDefined(self)

  /** Returns `this` context if defined or else the value of the `alternative`. */
  def orElse(alternative: => F[A]): F[A] = F.orElse(self)(alternative)

  /** Returns `true` if no value is defined within the context. */
  def isEmpty: Boolean = F.isEmpty(self)

  sealed trait Conditional[X] {
    def |(none: => X): X
  }

  /** Returns `some` if this context is defined, otherwise `none`. */
  def ?[X](some: => X): Conditional[X] = new Conditional[X] {
    def |(none: => X): X = F.?(self)(some,none)
  }

  /** Returns this context converted to the `Option` context. */
  def toOption: Option[A] = F.toOption(self)

}

trait ToOptionalOps {
  implicit def ToOptionalOps[F[_],A](v: F[A])(implicit F0: Optional[F]) =
    new OptionalOps[F,A] {
      def self = v
      implicit def F: Optional[F] = F0
    }
}
