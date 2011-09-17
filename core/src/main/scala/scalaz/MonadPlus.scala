package scalaz

trait MonadPlusLike[F[_]] extends MonadLike[F] with ApplicativePlusLike[F] { self =>
  ////
  def filter[A](fa: F[A])(f: A => Boolean) = bind(fa)(a => if (f(a)) pure(a) else empty[A])

  ////
  val monadPlusSyntax = new scalaz.syntax.MonadPlusSyntax[F] {}
}

////
/**
 *
 */
////
trait MonadPlus[F[_]] extends MonadPlusLike[F] {
  self : MonadInstance[F] with ApplicativePlusInstance[F] =>

  implicit val monadPlusParents: MonadInstance[F] with ApplicativePlusInstance[F] = this
}

object MonadPlus {
  def apply[F[_]](implicit F: MonadPlus[F]): MonadPlus[F] = F

  ////

  ////
}

trait MonadPlusInstance[F[_]] extends MonadPlus[F] with MonadInstance[F] with ApplicativePlusInstance[F]
