package scalaz

////
/**
 * @see [[scalaz.Monad.MonadLaw]]
 */
////
trait Monad[F[_]] extends Applicative[F] with Bind[F] { self =>
  ////

  override def map[A,B](fa: F[A])(f: A => B) = bind(fa)(a => point(f(a)))

  trait MonadLaw extends ApplicativeLaw {
    def rightIdentity[A](a: F[A])(implicit FA: Equal[F[A]]): Boolean = FA.equal(bind(a)(point(_: A)), a)
    def leftIdentity[A, B](a: A, f: A => F[B])(implicit FB: Equal[F[B]]): Boolean = FB.equal(bind(point(a))(f), f(a))
    def associativeBind[A, B, C](fa: F[A], f: A => F[B], g: B => F[C])(implicit FC: Equal[F[C]]): Boolean =
      FC.equal(bind(bind(fa)(f))(g), bind(fa)((a: A) => bind(f(a))(g)))
  }
  def monadLaw = new MonadLaw {}
  ////
  val monadSyntax = new scalaz.syntax.MonadSyntax[F] {}
}

object Monad {
  @inline def apply[F[_]](implicit F: Monad[F]): Monad[F] = F

  ////

  /** Performs the action  */
  def replicateM[F[_], G[_], A](a: F[A], n: Int)(implicit F: Monad[F], G: Traverse[G], P: Pointed[G], M: Monoid[G[F[A]]]): F[G[A]] =
    G.sequence(Monoid.replicate[G, F[A]](a)(n))

  def replicateM_[F[_], A](a: F[A], n: Int)(implicit F: Monad[F]): F[Unit] =
    if (n <= 0) F.point(()) else if (n == 1) F.point(a) else F.bind(F.point(a))(_ => replicateM_(a, n - 1))

  ////
}

