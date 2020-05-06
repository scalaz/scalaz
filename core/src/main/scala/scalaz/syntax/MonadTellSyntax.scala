package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `MonadTell` */
final class MonadTellOps[F[_], S, A] private[syntax](self: F[A])(implicit val F: MonadTell[F, S]) {
  ////

  final def :++>(w: => S): F[A] = F.bind(self)(a => F.map(F.tell(w))(_ => a))

  final def :++>>(f: A => S): F[A] =
    F.bind(self)(a => F.map(F.tell(f(a)))(_ => a))

  ////
}

trait ToMonadTellOps0[TC[F[_], S] <: MonadTell[F, S]] {
  implicit def ToMonadTellOps[F[_], S, A](v: F[A])(implicit F0: TC[F, S]): MonadTellOps[F, S, A] =
    new MonadTellOps[F, S, A](v)

  ////

  ////
}

trait ToMonadTellOps[TC[F[_], S] <: MonadTell[F, S]] extends ToMonadTellOps0[TC] with ToMonadOps[λ[F[_] => TC[F, S] forSome { type S }]]

trait MonadTellSyntax[F[_], S] extends MonadSyntax[F] {
  implicit def ToMonadTellOps[A](v: F[A]): MonadTellOps[F, S, A] =
    new MonadTellOps[F, S, A](v)(MonadTellSyntax.this.F)

  def F: MonadTell[F, S]
  ////

  ////
}
