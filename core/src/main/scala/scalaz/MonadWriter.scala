package scalaz

trait MonadWriter[F[_, _], W] extends Monad[({type f[+x] = F[W, x]})#f] {
  implicit def W: Monoid[W]
  
  def writer[A](v: (W, A)): F[W, A]
  def tell(w: W): F[W, Unit] = writer((w, ()))

  val monadWriterSyntax = new scalaz.syntax.MonadWriterSyntax[F, W]{}
}

object MonadWriter {
  def apply[F[+_, +_], W](implicit F: MonadWriter[F, W]) = F
}