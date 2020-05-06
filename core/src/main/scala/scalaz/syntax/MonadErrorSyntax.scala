package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `MonadError` */
final class MonadErrorOps[F[_], S, A] private[syntax](self: F[A])(implicit val F: MonadError[F, S]) {
  ////
  final def emap[B](f: A => S \/ B): F[B] =
    F.emap(self)(f)
  ////
}

trait ToMonadErrorOps0[TC[F[_], S] <: MonadError[F, S]] {
  implicit def ToMonadErrorOps[F[_], S, A](v: F[A])(implicit F0: TC[F, S]): MonadErrorOps[F, S, A] =
    new MonadErrorOps[F, S, A](v)

  ////

  implicit def ToMonadErrorIdOps[E](v: E): MonadErrorIdOps[E] =
    new MonadErrorIdOps[E](v)

  ////
}

trait ToMonadErrorOps[TC[F[_], S] <: MonadError[F, S]] extends ToMonadErrorOps0[TC] with ToMonadOps[λ[F[_] => TC[F, S] forSome { type S }]] with ToApplicativeErrorOps[TC]

trait MonadErrorSyntax[F[_], S] extends MonadSyntax[F] with ApplicativeErrorSyntax[F, S] {
  implicit def ToMonadErrorOps[A](v: F[A]): MonadErrorOps[F, S, A] =
    new MonadErrorOps[F, S, A](v)(MonadErrorSyntax.this.F)

  def F: MonadError[F, S]
  ////

  ////
}
