package scalaz

////
/**
 * Dual to [[scalaz.Functor]].  For example, functions provide a
 * [[scalaz.Functor]] in their result type, but a
 * [[scalaz.Contravariant]] for each argument type.
 *
 * Providing an instance of this is a useful alternative to marking a
 * type parameter with `-` in Scala.
 */
////
trait Contravariant[F[_]]  { self =>
  ////

  /** Transform `A`.
    *
    * @note `contramap(r)(identity)` = `r`
    */
  def contramap[A, B](r: F[A])(f: B => A): F[B]

  // derived functions

  ////
  val contravariantSyntax = new scalaz.syntax.ContravariantSyntax[F] { def F = Contravariant.this }
}

object Contravariant {
  @inline def apply[F[_]](implicit F: Contravariant[F]): Contravariant[F] = F

  ////

  ////
}
