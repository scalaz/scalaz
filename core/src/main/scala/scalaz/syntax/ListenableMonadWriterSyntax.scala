package scalaz
package syntax

trait ListenableMonadWriterOps[F[_, _], W, A] extends Ops[F[W, A]] {
  implicit def LMW: ListenableMonadWriter[F, W]

  final def written: F[W, W] = LMW.map(LMW.listen(self)){ case (_, w) => w }

  final def listen: F[W, (A, W)] = LMW.listen[A](self)
}

trait ToListenableMonadWriterOps0 {
  implicit def ToListenableMonadWriterOpsUnapply[FA](v: FA)(implicit F0: Unapply21[ListenableMonadWriter, FA]) =
    new ListenableMonadWriterOps[F0.M, F0.A, F0.B]{ def self = F0(v); implicit def LMW = F0.TC }
}

trait ToListenableMonadWriterOps extends ToListenableMonadWriterOps0 with ToMonadWriterOps {
  implicit def ToListenableMonadWriterOps[F[_, _], A, W](v: F[W, A])(implicit F0: ListenableMonadWriter[F, W]) = 
    new ListenableMonadWriterOps[F, W, A]{ def self = v; implicit def LMW = F0 }
}

trait ListenableMonadWriterSyntax[F[_, _], W] extends MonadWriterSyntax[F, W] {
  implicit def ToListenableMonadWriterOps[A](v: F[W, A])(implicit F0: ListenableMonadWriter[F, W]) =
    new ListenableMonadWriterOps[F, W, A]{ def self = v; implicit def LMW = F0 }
}

