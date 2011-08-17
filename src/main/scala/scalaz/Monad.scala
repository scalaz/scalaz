package scalaz

trait MonadLike[F[_]] extends ApplicativeLike[F] with BindLike[F] { derived =>
  override def map[A,B](fa: F[A])(f: A => B) = bind(fa)(a => pure(f(a)))
}
trait Monad[F[_]] extends MonadLike[F]
trait MonadInstance[F[_]] extends Monad[F] with BindInstance[F] with PointedInstance[F] 

trait ToMonadSyntax extends ToApplicativeSyntax with ToBindSyntax {
  // implicit def monad[F[_],A](v: F[A]) = (new MonadSyntax[F] {}).monadV(v)
}
trait MonadSyntax[F[_]] extends ApplicativeSyntax[F] with BindSyntax[F]
