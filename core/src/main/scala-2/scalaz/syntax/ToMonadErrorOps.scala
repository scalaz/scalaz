package scalaz
package syntax

trait ToMonadErrorOps[TC[F[_], S] <: MonadError[F, S]] extends ToMonadErrorOps0[TC] with ToMonadOps[λ[F[_] => TC[F, S] forSome { type S }]] with ToApplicativeErrorOps[TC]
