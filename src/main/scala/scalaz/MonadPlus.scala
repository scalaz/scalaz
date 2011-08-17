package scalaz

trait MonadPlusLike[F[_]] extends MonadLike[F] with ApplicativePlusLike[F] with BindLike[F] {
  def filter[A](fa: F[A])(f: A => Boolean) = bind(fa)(a => if (f(a)) pure(a) else empty[A])
}
  
trait MonadPlus[F[_]] extends MonadLike[F]
trait MonadPlusInstance[F[_]] extends MonadPlus[F] with MonadInstance[F] with PlusInstance[F]

trait ToMonadPlusSyntax extends ToMonadSyntax with ToPlusSyntax
trait MonadPlusSyntax[F[_]] extends MonadSyntax[F] with PlusSyntax[F]
