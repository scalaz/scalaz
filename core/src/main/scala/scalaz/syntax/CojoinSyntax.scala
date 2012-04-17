package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Cojoin` */
trait CojoinV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: Cojoin[F]
  ////
  final def cojoin: F[F[A]] = F.cojoin(self)
  ////
}

trait ToCojoinV0 {
  implicit def ToCojoinVUnapply[FA](v: FA)(implicit F0: Unapply[Cojoin, FA]) =
    new CojoinV[F0.M,F0.A] { def self = F0(v); implicit def F: Cojoin[F0.M] = F0.TC }

}

trait ToCojoinV extends ToCojoinV0 {
  implicit def ToCojoinV[F[_],A](v: F[A])(implicit F0: Cojoin[F]) =
    new CojoinV[F,A] { def self = v; implicit def F: Cojoin[F] = F0 }

  ////

  ////
}

trait CojoinSyntax[F[_]]  {
  implicit def ToCojoinV[A](v: F[A])(implicit F0: Cojoin[F]): CojoinV[F, A] = new CojoinV[F,A] { def self = v; implicit def F: Cojoin[F] = F0 }

  ////

  ////
}
