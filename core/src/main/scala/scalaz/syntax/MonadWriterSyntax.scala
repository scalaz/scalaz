package scalaz
package syntax

trait MonadWriterOps[F[_, _], W, A] extends Ops[F[W, A]] {
  implicit def MW: MonadWriter[F, W]

  final def :++>(w: => W): F[W, A] = MW.bind(self)(a => MW.map(MW.tell(w))(_ => a))

  final def :++>>(f: A => W): F[W, A] =
     MW.bind(self)(a => MW.map(MW.tell(f(a)))(_ => a))
}

trait ToMonadWriterOps0 {
  implicit def ToMonadWriterOpsUnapply[FA](v: FA)(implicit F0: Unapply21[MonadWriter, FA]) = 
    new MonadWriterOps[F0.M, F0.A, F0.B]{ def self = F0(v); implicit def MW = F0.TC }
}

trait ToMonadWriterOps extends ToMonadWriterOps0 {
  implicit def ToMonadWriterOps[F[_, _], A, W](v: F[W, A])(implicit F0: MonadWriter[F, W]) = 
    new MonadWriterOps[F, W, A]{ def self = v; implicit def MW = F0 }
}

trait MonadWriterSyntax[F[_, _], W] {
  implicit def ToMonadWriterOps[A](v: F[W, A])(implicit F0: MonadWriter[F, W]) =
    new MonadWriterOps[F, W, A]{ def self = v; implicit def MW = F0 }
}
