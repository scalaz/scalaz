package scalaz

trait ListenableMonadWriter[F[+_, +_], +W] extends MonadWriter[F, W] {
  def listen[A](ma: F[W, A]): F[W, (A, W)]

  def pass[A](ma: F[W, (A, W => W)]): F[W, A] =
    bind(listen(ma)){ case ((a, f), w) => writer(f(w), a) }

  val listenableMonadWriterSyntax = new scalaz.syntax.ListenableMonadWriterSyntax[F, W]{}
}

object ListenableMonadWriter {
  def apply[F[_, _], W](implicit LMW: ListenableMonadWriter[F, W]) = LMW
}
