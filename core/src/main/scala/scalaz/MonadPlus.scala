package scalaz

////
/**
 *
 */
////
trait MonadPlus[F[_]] extends Monad[F] with ApplicativePlus[F] { self =>
  ////
  def filter[A](fa: F[A])(f: A => Boolean) = bind(fa)(a => if (f(a)) point(a) else empty[A])

  trait MonadPlusLaw extends EmptyLaw with MonadLaw {
    def leftBindIdentity[A](f: A => F[A])(implicit FA: Equal[F[A]]): Boolean = {
      FA.equal(bind(empty[A])(f), empty[A])
    }
    def rightBindIdentity[A](f: F[A])(implicit FA: Equal[F[A]]): Boolean = {
      FA.equal(bind(f)(_ => empty[A]), f)
    }
  }
  def monadPlusLaw = new MonadPlusLaw {}
  ////
  val monadPlusSyntax = new scalaz.syntax.MonadPlusSyntax[F] {}
}

object MonadPlus {
  @inline def apply[F[_]](implicit F: MonadPlus[F]): MonadPlus[F] = F

  ////

  ////
}

