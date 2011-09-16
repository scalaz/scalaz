package scalaz

trait ContravariantLike[F[_]]  { self =>
  ////
  def contramap[A, B](r: F[A], f: B => A): F[B]

  // derived functions

  ////
  val contravariantSyntax = new scalaz.syntax.ContravariantSyntax[F] {}
}

////
/**
 *
 */
////
trait Contravariant[F[_]] extends ContravariantLike[F]

trait ContravariantInstance[F[_]] extends Contravariant[F]
