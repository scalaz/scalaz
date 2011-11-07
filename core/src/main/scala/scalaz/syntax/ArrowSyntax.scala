package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Arrow` */
trait ArrowV[F[_, _],A, B] extends SyntaxV[F[A, B]] {
  implicit def F: Arrow[F]
  ////
  final def first[C]: F[(A, C), (B, C)] =
    F.first(self)

  final def second[C]: F[(C, A), (C, B)] =
    F.second(self)

  final def ***[C, D](k: F[C, D]): F[(A, C), (B, D)] =
    F.split(self, k)

  final def &&&[C](k: F[A, C]): F[A, (B, C)] =
    F.combine(self, k)

  final def product: F[(A, A), (B, B)] =
    ***(self)

  final def ^>>[C](f: C => A): F[C, B] =
    F.mapfst(self)(f)

  final def >>^[C](f: B => C): F[A, C] =
    F.mapsnd(self)(f)

  ////
}

trait ToArrowV extends ToCategoryV with ToArrV with ToFirstV {
    implicit def ToArrowV[FA](v: FA)(implicit F0: Unapply2[Arrow, FA]) =
      new ArrowV[F0.M,F0.A,F0.B] { def self = F0(v); implicit def F: Arrow[F0.M] = F0.TC }
  

  ////

  ////
}

trait ArrowSyntax[F[_, _]] extends CategorySyntax[F] with ArrSyntax[F] with FirstSyntax[F] {
  implicit def ToArrowV[A, B](v: F[A, B])(implicit F0: Arrow[F]): ArrowV[F, A, B] = new ArrowV[F, A, B] { def self = v; implicit def F: Arrow[F] = F0 }

  ////

  ////
}
