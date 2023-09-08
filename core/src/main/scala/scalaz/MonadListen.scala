package scalaz

import scalaz.syntax.MonadListenSyntax

trait MonadListen[F[_], W] extends MonadTell[F, W] {
  def listen[A](ma: F[A]): F[(A, W)]

  def pass[A](ma: F[(A, W => W)]): F[A] =
    bind(listen(ma)){ case ((a, f), w) => writer(f(w), a) }

  val monadListenSyntax: MonadListenSyntax[F, W] = new MonadListenSyntax[F, W]{
    def F: MonadListen[F, W] = MonadListen.this
  }
}

object MonadListen {
  def apply[F[_], W](implicit ML: MonadListen[F, W]) = ML
}
