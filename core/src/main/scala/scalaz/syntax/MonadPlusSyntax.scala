package scalaz
package syntax


trait ToMonadPlusSyntax extends ToMonadSyntax with ToPlusSyntax

trait MonadPlusSyntax[F[_]] extends MonadSyntax[F] with PlusSyntax[F]
