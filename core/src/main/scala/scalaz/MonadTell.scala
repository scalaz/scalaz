package scalaz

/** The class of monads supporting write operations
  */
trait MonadTell[F[_, _], W] extends Monad[({type λ[+α] = F[W, α]})#λ] {
  def writer[A](w: W, v: A): F[W, A]

  def tell(w: W): F[W, Unit] = writer(w, ())

  val monadTellSyntax = new scalaz.syntax.MonadTellSyntax[F, W]{}
}

object MonadTell {
  def apply[F[+_, +_], W](implicit F: MonadTell[F, W]) = F
}
