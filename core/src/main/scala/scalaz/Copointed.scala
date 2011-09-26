package scalaz

trait Copointed[F[_]] extends Contravariant[F] { self =>
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

object Copointed {
  def apply[F[_]](implicit F: Copointed[F]): Copointed[F] = F

  ////

  ////
}

