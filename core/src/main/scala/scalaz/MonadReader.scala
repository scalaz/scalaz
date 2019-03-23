package scalaz

////
/**
 *
 */
////
trait MonadReader[F[_], S] extends Monad[F] { self =>
  ////

  def ask: F[S]
  def local[A](f: S => S)(fa: F[A]): F[A]
  def scope[A](k: S)(fa: F[A]): F[A] = local(_ => k)(fa)
  def asks[A](f: S => A): F[A] = map(ask)(f)

  trait MonadReaderLaw extends MonadLaw {
    def localPoint[A](f: S => S)(a: A)(implicit FA: Equal[F[A]]): Boolean =
      FA.equal(local(f)(point(a)), point(a))

    def localComposition[A](f: S => S, g: S => S)(fa: F[A])(implicit FA: Equal[F[A]]): Boolean =
      FA.equal(local(f)(local(g)(fa)), local(f andThen g)(fa))

    def localFAsk(f: S => S)(implicit FS: Equal[F[S]]): Boolean =
      FS.equal(local(f)(ask), map(ask)(f))

    def askIdempotence(implicit FS: Equal[F[S]]): Boolean =
      FS.equal(ask, apply2(ask, ask)((_, b) => b))

    def askFALeft[A](fa: F[A])(implicit FA: Equal[F[A]]): Boolean =
      FA.equal(apply2(ask, fa)((_, b) => b), fa)

    def askFARight[A](fa: F[A])(implicit FA: Equal[F[A]]): Boolean =
      FA.equal(apply2(fa, ask)((a, _) => a), fa)
  }

  def monadReaderLaw = new MonadReaderLaw {}

  ////

}

object MonadReader {
  @inline def apply[F[_], S](implicit F: MonadReader[F, S]): MonadReader[F, S] = F

  import Isomorphism._

  def fromIso[F[_], G[_], E](D: F <~> G)(implicit A: MonadReader[G, E]): MonadReader[F, E] =
    new IsomorphismMonadReader[F, G, E] {
      override def G: MonadReader[G, E] = A
      override def iso: F <~> G = D
    }

  ////

  ////
}

trait IsomorphismMonadReader[F[_], G[_], S] extends MonadReader[F, S] with IsomorphismMonad[F, G]{
  implicit def G: MonadReader[G, S]
  ////

  override def ask: F[S] = iso.from(G.ask)

  override def local[A](f: S => S)(fa: F[A]): F[A] =
    iso.from(G.local(f)(iso.to(fa)))
  ////
}
