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

  import Isomorphism._

  def fromIso[F[_], G[_], E](D: F <~> G)(implicit A: MonadTell[G, E]): MonadTell[F, E] =
    new IsomorphismMonadTell[F, G, E] {
      override def G: MonadTell[G, E] = A
      override def iso: F <~> G = D
    }

  ////

  ////
}
