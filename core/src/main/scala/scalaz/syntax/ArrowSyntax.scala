package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Arrow` */
trait ArrowV[F[_, _],A, B] extends SyntaxV[F[A, B]] {
  ////
  def first[C](implicit a: Arrow[F]): F[(A, C), (B, C)] =
    a.first(self)

  def second[C](implicit a: Arrow[F]): F[(C, A), (C, B)] =
    a.second(self)

  def ***[C, D](k: F[C, D])(implicit a: Arrow[F]): F[(A, C), (B, D)] =
    a.split(self, k)

  def &&&[C](k: F[A, C])(implicit a: Arrow[F]): F[A, (B, C)] =
    a.combine(self, k)

  def product(implicit a: Arrow[F]): F[(A, A), (B, B)] =
    ***(self)

  def ^>>[C](f: C => A)(implicit a: Arrow[F]): F[C, B] =
    a.mapfst(self)(f)

  def >>^[C](f: B => C)(implicit a: Arrow[F]): F[A, C] =
    a.mapsnd(self)(f)

  ////
}

trait ToArrowSyntax extends ToCategorySyntax with ToArrSyntax with ToFirstSyntax {
  implicit def ToArrowV[F[_, _],A, B](v: F[A, B]) =
    new ArrowV[F,A, B] { def self = v }

  ////

  ////
}

trait ArrowSyntax[F[_, _]] extends CategorySyntax[F] with ArrSyntax[F] with FirstSyntax[F] {
  implicit def ToArrowV[A, B](v: F[A, B]): ArrowV[F, A, B] = new ArrowV[F, A, B] { def self = v }

  ////

  ////
}
