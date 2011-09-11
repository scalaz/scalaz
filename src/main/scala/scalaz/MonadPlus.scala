package scalaz

trait MonadPlusLike[F[_]] extends MonadLike[F] with ApplicativePlusLike[F] with BindLike[F] {
  def filter[A](fa: F[A])(f: A => Boolean) = bind(fa)(a => if (f(a)) pure(a) else empty[A])

  override val syntax = new scalaz.syntax.MonadPlusSyntax[F] {}
}
  
trait MonadPlus[F[_]] extends MonadPlusLike[F]
trait MonadPlusInstance[F[_]] extends MonadPlus[F] with MonadInstance[F] with PlusInstance[F]
