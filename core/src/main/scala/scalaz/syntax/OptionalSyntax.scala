package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Optional` */
final class OptionalOps[F[_],A] private[syntax](val self: F[A])(implicit val F: Optional[F]) extends Ops[F[A]] {
  ////

  /** If the value has an `a`, return it; otherwise it must be
    * universally quantified.
    */
  final def pextract[B]: F[B] \/ A = F.pextract(self)

  /** Returns the value within the context if defined or else the value of `default`. */
  def getOrElse(default: => A): A = F.getOrElse(self)(default)

  /** Returns `true` if a value is defined within the context. */
  def isDefined: Boolean = F.isDefined(self)

  /** Returns `this` context if defined or else the value of the `alternative`. */
  def orElse(alternative: => F[A]): F[A] = F.orElse(self)(alternative)

  /** Returns `true` if a value is defined within the context. */
  def nonEmpty: Boolean = F.nonEmpty(self)

  /** Returns `true` if no value is defined within the context. */
  def isEmpty: Boolean = F.isEmpty(self)

  final class Conditional[X](some: => X) {
    def |(none: => X): X = F.?(self)(some,none)
  }

  /** Returns `some` if this context is defined, otherwise `none`. */
  def ?[X](some: => X): Conditional[X] = new Conditional[X](some)

  /** Returns this context converted to the `Option` context. */
  def toOption: Option[A] = F.toOption(self)

  /** Returns this context converted to the `Maybe` context. */
  def toMaybe: Maybe[A] = F.toMaybe(self)

  ////
}

sealed trait ToOptionalOps0 {
  implicit def ToOptionalOpsUnapply[FA](v: FA)(implicit F0: Unapply[Optional, FA]): OptionalOps[F0.M, F0.A] =
    new OptionalOps[F0.M, F0.A](F0(v))(F0.TC)

}

trait ToOptionalOps extends ToOptionalOps0 {
  implicit def ToOptionalOps[F[_], A](v: F[A])(implicit F0: Optional[F]): OptionalOps[F, A] =
    new OptionalOps[F, A](v)

  ////

  ////
}

trait OptionalSyntax[F[_]]  {
  implicit def ToOptionalOps[A](v: F[A]): OptionalOps[F, A] = new OptionalOps[F,A](v)(OptionalSyntax.this.F)

  def F: Optional[F]
  ////

  ////
}
