package scalaz

trait MonadListen[F[_, _], W] extends MonadTell[F, W] {
  def listen[A](ma: F[W, A]): F[W, (A, W)]

  def pass[A](ma: F[W, (A, W => W)]): F[W, A] =
    bind(listen(ma)){ case ((a, f), w) => writer(f(w), a) }

  val monadListenSyntax = new scalaz.syntax.MonadListenSyntax[F, W]{}
}

object MonadListen {
  def apply[F[_, _], W](implicit ML: MonadListen[F, W]) = ML
}
