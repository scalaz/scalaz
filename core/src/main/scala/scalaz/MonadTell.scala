package scalaz

////
/** The class of monads supporting write operations
 *
 */
////
trait MonadTell[F[_, _], S] extends Monad[F[S, ?]] { self =>
  ////
  def writer[A](w: S, v: A): F[S, A]

  def tell(w: S): F[S, Unit] = writer(w, ())

  ////
  val monadTellSyntax = new scalaz.syntax.MonadTellSyntax[F, S] { def F = MonadTell.this }
}

object MonadTell {
  @inline def apply[F[_, _], S](implicit F: MonadTell[F, S]): MonadTell[F, S] = F

  ////

  ////
}
