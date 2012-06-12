package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `ArrId` */
trait ArrIdOps[F[_, _],A, B] extends Ops[F[A, B]] {
  implicit def F: ArrId[F]
  ////

  ////
}

trait ToArrIdOps0 {
    implicit def ToArrIdOpsUnapply[FA](v: FA)(implicit F0: Unapply2[ArrId, FA]) =
      new ArrIdOps[F0.M,F0.A,F0.B] { def self = F0(v); implicit def F: ArrId[F0.M] = F0.TC }
  
}

trait ToArrIdOps extends ToArrIdOps0 {
  
  implicit def ToArrIdOps[F[_, _],A, B](v: F[A, B])(implicit F0: ArrId[F]) =
      new ArrIdOps[F,A, B] { def self = v; implicit def F: ArrId[F] = F0 }
  

  ////

  ////
}

trait ArrIdSyntax[F[_, _]]  {
  implicit def ToArrIdOps[A, B](v: F[A, B])(implicit F0: ArrId[F]): ArrIdOps[F, A, B] = new ArrIdOps[F, A, B] { def self = v; implicit def F: ArrId[F] = F0 }

  ////

  ////
}
