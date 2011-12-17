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
    def leftBindIdentity[A, B](f: A => F[A])(implicit F: Bind[F], FA: Equal[F[A]]): Boolean = {
      FA.equal(F.bind(empty[A])(f), empty[A])
    }
    def rightBindIdentity[A, B](f: F[A])(implicit F: Bind[F], FA: Equal[F[A]]): Boolean = {
      FA.equal(F.bind(f)(_ => empty[A]), f)
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

