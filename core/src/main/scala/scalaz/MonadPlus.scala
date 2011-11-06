package scalaz

trait MonadPlus[F[_]] extends Monad[F] with ApplicativePlus[F] { self =>
  ////
  def filter[A](fa: F[A])(f: A => Boolean) = bind(fa)(a => if (f(a)) point(a) else empty[A])

  ////
  val monadPlusSyntax = new scalaz.syntax.MonadPlusSyntax[F] {}
}

object MonadPlus {
  @inline def apply[F[_]](implicit F: MonadPlus[F]): MonadPlus[F] = F

  ////

  ////
}

