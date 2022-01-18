package scalaz
package syntax

trait ToApplicativeErrorOps[TC[F[_], S] <: ApplicativeError[F, S]] extends ToApplicativeErrorOps0[TC] with ToApplicativeOps[λ[F[_] => TC[F, S] forSome { type S }]]
