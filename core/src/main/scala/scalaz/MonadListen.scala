package scalaz

trait MonadListen[F[_], W] extends MonadTell[F, W] {
  def listen[A](ma: F[A]): F[(A, W)]

  def pass[A](ma: F[(A, W => W)]): F[A] =
    monad.bind(listen(ma)){ case ((a, f), w) => writer(f(w), a) }

  val monadListenSyntax = new scalaz.syntax.MonadListenSyntax[F, W]{ def FS = MonadListen.this }
}

object MonadListen {
  def apply[F[_], W](implicit ML: MonadListen[F, W]) = ML
}
