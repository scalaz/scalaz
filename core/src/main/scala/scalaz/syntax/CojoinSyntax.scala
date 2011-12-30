package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `CoJoin` */
trait CoJoinV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: CoJoin[F]
  ////
  final def cojoin: F[F[A]] = F.cojoin(self)
  ////
}

trait ToCoJoinV0 {
  implicit def ToCoJoinVUnapply[FA](v: FA)(implicit F0: Unapply[CoJoin, FA]) =
    new CoJoinV[F0.M,F0.A] { def self = F0(v); implicit def F: CoJoin[F0.M] = F0.TC }

}

trait ToCoJoinV extends ToCoJoinV0 {
  implicit def ToCoJoinV[F[_],A](v: F[A])(implicit F0: CoJoin[F]) =
    new CoJoinV[F,A] { def self = v; implicit def F: CoJoin[F] = F0 }

  ////

  ////
}

trait CoJoinSyntax[F[_]]  {
  implicit def ToCoJoinV[A](v: F[A])(implicit F0: CoJoin[F]): CoJoinV[F, A] = new CoJoinV[F,A] { def self = v; implicit def F: CoJoin[F] = F0 }

  ////

  ////
}
