package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Arrow` */
trait ArrowV[F[_, _],A, B] extends SyntaxV[F[A, B]] {
  implicit def F: Arrow[F]
  ////
  def first[C]: F[(A, C), (B, C)] =
    F.first(self)

  def second[C]: F[(C, A), (C, B)] =
    F.second(self)

  def ***[C, D](k: F[C, D]): F[(A, C), (B, D)] =
    F.split(self, k)

  def &&&[C](k: F[A, C]): F[A, (B, C)] =
    F.combine(self, k)

  def product: F[(A, A), (B, B)] =
    ***(self)

  def ^>>[C](f: C => A): F[C, B] =
    F.mapfst(self)(f)

  def >>^[C](f: B => C): F[A, C] =
    F.mapsnd(self)(f)

  ////
}

trait ToArrowSyntax extends ToCategorySyntax with ToArrSyntax with ToFirstSyntax {
  implicit def ToArrowV[F[_, _],A, B](v: F[A, B])(implicit F0: Arrow[F]) =
    new ArrowV[F,A, B] { def self = v; implicit def F: Arrow[F] = F0 }

  ////

  ////
}

trait ArrowSyntax[F[_, _]] extends CategorySyntax[F] with ArrSyntax[F] with FirstSyntax[F] {
  implicit def ToArrowV[A, B](v: F[A, B])(implicit F0: Arrow[F]): ArrowV[F, A, B] = new ArrowV[F, A, B] { def self = v; implicit def F: Arrow[F] = F0 }

  ////

  ////
}
