package scalaz
package syntax

trait ToMonadTellOps[TC[F[_], S] <: MonadTell[F, S]] extends ToMonadTellOps0[TC] with ToMonadOps[λ[F[_] => TC[F, S] forSome { type S }]]
