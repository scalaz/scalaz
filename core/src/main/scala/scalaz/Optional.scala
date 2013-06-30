package scalaz

////
/** Abstraction over a container/context which may or may not provide a value.
  *
  * @tparam F the container/context type
  *
  * @see [[syntax.OptionalOps]]
  */
////
trait Optional[F[_]]  { self =>
  ////

  /** Returns the value within the context if defined or else the value of `default`. */
  def getOrElse[A](fa: F[A])(default: => A): A

  /** Returns `true` if a value is defined within the context. */
  def isDefined[A](fa: F[A]): Boolean

  /** Returns `this` context if defined or else the value of the `alternative`. */
  def orElse[A](fa: F[A])(alternative: => F[A]): F[A]

  // derived functions

  /** Returns `true` if no value is defined within the context. */
  def isEmpty[A](fa: F[A]): Boolean = ! isDefined(fa)

  /** Returns `some` if this context is defined, otherwise `none`. */
  def ?[A,X](fa: F[A])(some: => X, none: => X): X =
    if (isDefined(fa)) some else none

  // conversions

  /** Returns this context converted to the `Option` context. */
  def toOption[A](fa: F[A]): Option[A]

  ////
  val optionalSyntax = new scalaz.syntax.OptionalSyntax[F] { def F = Optional.this }
}

object Optional {
  @inline def apply[F[_]](implicit F: Optional[F]): Optional[F] = F

  ////

  ////
}
