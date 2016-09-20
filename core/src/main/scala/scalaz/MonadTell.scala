package scalaz

////
/** The class of monads supporting write operations
 *
 */
////
trait MonadTell[F[_], S] {
  ////
  def monadInstance: Monad[F]

  def writer[A](w: S, v: A): F[A]

  def tell(w: S): F[Unit] = writer(w, ())

  ////
  val monadTellSyntax = new scalaz.syntax.MonadTellSyntax[F, S] { def FS = MonadTell.this }
}

object MonadTell {
  @inline def apply[F[_], S](implicit F: MonadTell[F, S]): MonadTell[F, S] = F

  ////

  ////
}
