package scalaz

////
/**
 *
 */
////
trait ApplicativePlus[F[_]] extends Applicative[F] with PlusEmpty[F] { self =>
  ////

  /**The composition of ApplicativePlus `F` and `G`, `[x]F[G[x]]`, is a ApplicativePlus */
  def compose[G[_]](implicit G0: ApplicativePlus[G]): ApplicativePlus[({type λ[α] = F[G[α]]})#λ] = new CompositionApplicativePlus[F, G] {
    implicit def F = self

    implicit def G = G0
  }

  ////
  val applicativePlusSyntax = new scalaz.syntax.ApplicativePlusSyntax[F] {}
}

object ApplicativePlus {
  @inline def apply[F[_]](implicit F: ApplicativePlus[F]): ApplicativePlus[F] = F

  ////

  ////
}

