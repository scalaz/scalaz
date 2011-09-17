package scalaz

trait CopointedLike[F[_]] extends ContravariantLike[F] { self =>
  ////

  // derived functions

  ////
  val copointedSyntax = new scalaz.syntax.CopointedSyntax[F] {}
}

////
/**
 *
 */
////
trait Copointed[F[_]] extends CopointedLike[F]

object Copointed {
  def apply[F[_]](implicit F: Copointed[F]): Copointed[F] = F

  ////

  ////
}

trait CopointedInstance[F[_]] extends Copointed[F] with ContravariantInstance[F]
