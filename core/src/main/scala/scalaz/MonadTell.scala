package scalaz

////
/** The class of monads supporting write operations
 *
 */
////
trait MonadTell[F[_], S] extends Monad[F] { self =>
  ////
  def writer[A](w: S, v: A): F[A]

  def tell(w: S): F[Unit] = writer(w, ())

  ////
  val monadTellSyntax = new scalaz.syntax.MonadTellSyntax[F, S] { def F = MonadTell.this }
}

object MonadTell {
  @inline def apply[F[_], S](implicit F: MonadTell[F, S]): MonadTell[F, S] = F

  ////
  import Isomorphism.<~>

  def fromIso[F[_], G[_], S](D: F <~> G)(implicit E: MonadTell[G, S]): MonadTell[F, S] =
    new IsomorphismMonadTell[F, G, S] {
      override implicit def G: MonadTell[G, S] = E
      override def iso: F <~> G = D
    }
  ////
}
