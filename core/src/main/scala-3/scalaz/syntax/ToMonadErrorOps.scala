package scalaz
package syntax

trait ToMonadErrorOps[TC[F[_], S] <: MonadError[F, S]] extends ToMonadErrorOps0[TC] with ToApplicativeErrorOps[TC]
