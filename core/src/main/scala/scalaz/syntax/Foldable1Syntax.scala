package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Foldable1` */
sealed abstract class Foldable1Ops[F[_],A] extends Ops[F[A]] {
  implicit def F: Foldable1[F]
  ////
  final def foldRight1(f: (A, => A) => A): A = F.foldRight1(self)(f)
  final def foldLeft1(f: (A, A) => A): A = F.foldLeft1(self)(f)
  final def foldr1(f: A => (=> A) => A): A = F.foldr1(self)(f)
  final def foldl1(f: A => A => A): A = F.foldl1(self)(f)
  final def foldMap1[B: Semigroup](f: A => B = (a: A) => a): B = F.foldMap1(self)(f)
  final def sumr1(implicit A: Semigroup[A]): A = F.foldRight1(self)(A.append)
  final def suml1(implicit A: Semigroup[A]): A = F.foldLeft1(self)(A.append(_, _))
  final def maximum1(implicit A: Order[A]): A = F.maximum1(self)
  final def maximumOf1[B: Order](f: A => B): B = F.maximumOf1(self)(f)
  final def maximumBy1[B: Order](f: A => B): A = F.maximumBy1(self)(f)
  final def minimum1(implicit A: Order[A]): A = F.minimum1(self)
  final def minimumOf1[B: Order](f: A => B): B = F.minimumOf1(self)(f)
  final def minimumBy1[B: Order](f: A => B): A = F.minimumBy1(self)(f)
  ////
}

trait ToFoldable1Ops0 {
  implicit def ToFoldable1OpsUnapply[FA](v: FA)(implicit F0: Unapply[Foldable1, FA]) =
    new Foldable1Ops[F0.M,F0.A] { def self = F0(v); implicit def F: Foldable1[F0.M] = F0.TC }

}

trait ToFoldable1Ops extends ToFoldable1Ops0 with ToFoldableOps {
  implicit def ToFoldable1Ops[F[_],A](v: F[A])(implicit F0: Foldable1[F]) =
    new Foldable1Ops[F,A] { def self = v; implicit def F: Foldable1[F] = F0 }

  ////

  ////
}

trait Foldable1Syntax[F[_]] extends FoldableSyntax[F] {
  implicit def ToFoldable1Ops[A](v: F[A]): Foldable1Ops[F, A] = new Foldable1Ops[F,A] { def self = v; implicit def F: Foldable1[F] = Foldable1Syntax.this.F }

  def F: Foldable1[F]
  ////

  ////
}
