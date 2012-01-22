package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `ArrId` */
trait ArrIdV[F[_, _],A, B] extends SyntaxV[F[A, B]] {
  implicit def F: ArrId[F]
  ////

  ////
}

trait ToArrIdV0 {
    implicit def ToArrIdVUnapply[FA](v: FA)(implicit F0: Unapply2[ArrId, FA]) =
      new ArrIdV[F0.M,F0.A,F0.B] { def self = F0(v); implicit def F: ArrId[F0.M] = F0.TC }
  
}

trait ToArrIdV extends ToArrIdV0 {
  
  implicit def ToArrIdV[F[_, _],A, B](v: F[A, B])(implicit F0: ArrId[F]) =
      new ArrIdV[F,A, B] { def self = v; implicit def F: ArrId[F] = F0 }
  

  ////

  ////
}

trait ArrIdSyntax[F[_, _]]  {
  implicit def ToArrIdV[A, B](v: F[A, B])(implicit F0: ArrId[F]): ArrIdV[F, A, B] = new ArrIdV[F, A, B] { def self = v; implicit def F: ArrId[F] = F0 }

  ////

  ////
}
