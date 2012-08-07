package scalaz

trait MonadWriter[F[+_, +_], +W] extends Monad[({type f[+x] = F[W, x]})#f] {
  implicit def W: Monoid[W]
  
  def writer[A, WW >: W](v: (WW, A)): F[WW, A]
  def tell[WW >: W](w: WW): F[WW, Unit] = writer((w, ()))

  val monadWriterSyntax = new scalaz.syntax.MonadWriterSyntax[F, W]{}
}

object MonadWriter {
  def apply[F[_, _], W](implicit F: MonadWriter[F, W]) = F
}
