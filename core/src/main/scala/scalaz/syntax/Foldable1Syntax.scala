package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Foldable1` */
final class Foldable1Ops[F[_],A] private[syntax](val self: F[A])(implicit val F: Foldable1[F]) extends Ops[F[A]] {
  ////
  import Leibniz.===

  final def foldMapRight1[B](z: A => B)(f: (A, => B) => B): B = F.foldMapRight1(self)(z)(f)
  final def foldMapLeft1[B](z: A => B)(f: (B, A) => B): B = F.foldMapLeft1(self)(z)(f)
  final def foldRight1(f: (A, => A) => A): A = F.foldRight1(self)(f)
  final def foldLeft1(f: (A, A) => A): A = F.foldLeft1(self)(f)
  final def foldr1(f: A => (=> A) => A): A = F.foldr1(self)(f)
  final def foldl1(f: A => A => A): A = F.foldl1(self)(f)
  final def foldMap1[B: Semigroup](f: A => B = (a: A) => a): B = F.foldMap1(self)(f)
  final def sumr1(implicit A: Semigroup[A]): A = F.sumr1(self)
  final def suml1(implicit A: Semigroup[A]): A = F.suml1(self)
  final def maximum1(implicit A: Order[A]): A = F.maximum1(self)
  final def maximumOf1[B: Order](f: A => B): B = F.maximumOf1(self)(f)
  final def maximumBy1[B: Order](f: A => B): A = F.maximumBy1(self)(f)
  final def minimum1(implicit A: Order[A]): A = F.minimum1(self)
  final def minimumOf1[B: Order](f: A => B): B = F.minimumOf1(self)(f)
  final def minimumBy1[B: Order](f: A => B): A = F.minimumBy1(self)(f)
  final def distinct1(implicit A: Order[A]): NonEmptyList[A] = F.distinct1(self)
  final def distinctE1(implicit A: Equal[A]): NonEmptyList[A] = F.distinctE1(self)
  final def intercalate1(a: A)(implicit A: Semigroup[A]): A = F.intercalate1(self, a)
  final def msuml1[G[_], B](implicit ev: A === G[B], G: Plus[G]): G[B] = F.msuml1(ev.subst[F](self))
  final def toNel: NonEmptyList[A] = F.toNel(self)
  final def scanLeft1(f: (A, A) => A): NonEmptyList[A] = F.scanLeft1(self)(f)
  final def scanRight1(f: (A, A) => A): NonEmptyList[A] = F.scanRight1(self)(f)
  ////
}

sealed trait ToFoldable1Ops0 {
  implicit def ToFoldable1OpsUnapply[FA](v: FA)(implicit F0: Unapply[Foldable1, FA]): Foldable1Ops[F0.M, F0.A] =
    new Foldable1Ops[F0.M, F0.A](F0(v))(F0.TC)

}

trait ToFoldable1Ops extends ToFoldable1Ops0 with ToFoldableOps {
  implicit def ToFoldable1Ops[F[_], A](v: F[A])(implicit F0: Foldable1[F]): Foldable1Ops[F, A] =
    new Foldable1Ops[F, A](v)

  ////

  ////
}

trait Foldable1Syntax[F[_]] extends FoldableSyntax[F] {
  implicit def ToFoldable1Ops[A](v: F[A]): Foldable1Ops[F, A] = new Foldable1Ops[F,A](v)(Foldable1Syntax.this.F)

  def F: Foldable1[F]
  ////

  ////
}
