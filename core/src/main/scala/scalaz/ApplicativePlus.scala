package scalaz

////
/**
 *
 */
////
trait ApplicativePlus[F[_]] extends Applicative[F] with Empty[F] { self =>
  ////

  ////
  val applicativePlusSyntax = new scalaz.syntax.ApplicativePlusSyntax[F] {}
}

object ApplicativePlus {
  @inline def apply[F[_]](implicit F: ApplicativePlus[F]): ApplicativePlus[F] = F

  ////

  ////
}

