package scalaz
package syntax


trait ToMonadSyntax extends ToApplicativeSyntax with ToBindSyntax {
  // implicit def monad[F[_],A](v: F[A]) = (new MonadSyntax[F] {}).monadV(v)
}

trait MonadSyntax[F[_]] extends ApplicativeSyntax[F] with BindSyntax[F]
