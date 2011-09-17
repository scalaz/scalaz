package scalaz

trait ApplicativePlusLike[F[_]] extends ApplicativeLike[F] with PlusLike[F] { self =>
  ////

  ////
  val applicativePlusSyntax = new scalaz.syntax.ApplicativePlusSyntax[F] {}
}

////
/**
 *
 */
////
trait ApplicativePlus[F[_]] extends ApplicativePlusLike[F] {
  self : ApplicativeInstance[F] with PlusInstance[F] =>

  implicit val applicativePlusParents: ApplicativeInstance[F] with PlusInstance[F] = this
}

object ApplicativePlus {
  def apply[F[_]](implicit F: ApplicativePlus[F]): ApplicativePlus[F] = F

  ////

  ////
}

trait ApplicativePlusInstance[F[_]] extends ApplicativePlus[F] with ApplicativeInstance[F] with PlusInstance[F]
