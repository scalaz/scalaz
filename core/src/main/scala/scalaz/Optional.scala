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

  /** If `fa` has an `a`, return it; otherwise it must be universally
    * quantified.
    */
  def pextract[B, A](fa: F[A]): F[B] \/ A

  // derived functions

  /** Returns the value within the context if defined or else the value of `default`. */
  def getOrElse[A](fa: F[A])(default: => A): A = pextract(fa) | default

  /** Returns `true` if a value is defined within the context. */
  def isDefined[A](fa: F[A]): Boolean = pextract(fa).isRight

  /** Returns `true` if a value is defined within the context. This is an alias for `isDefined`. */
  final def nonEmpty[A](fa: F[A]): Boolean = isDefined(fa)

  /** Returns `true` if no value is defined within the context. */
  final def isEmpty[A](fa: F[A]): Boolean = ! isDefined(fa)

  /** Returns given context if it is defined or else the value of the `alternative`. */
  def orElse[A](fa: F[A])(alternative: => F[A]): F[A] =
    if (isDefined(fa)) fa else alternative

  /** Returns `some` if this context is defined, otherwise `none`. */
  def ?[A,X](fa: F[A])(some: => X, none: => X): X =
    if (isDefined(fa)) some else none

  // conversions

  /** Returns this context converted to the `Option` context. */
  def toOption[A](fa: F[A]): Option[A] = pextract(fa).toOption

  ////
  val optionalSyntax = new scalaz.syntax.OptionalSyntax[F] { def F = Optional.this }
}

object Optional {
  @inline def apply[F[_]](implicit F: Optional[F]): Optional[F] = F

  ////

  ////
}
