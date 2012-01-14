package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Foldable` */
trait FoldableV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: Foldable[F]
  ////
  final def foldMap[B: Monoid](f: A => B = (a: A) => a): B = F.foldMap(self)(f)
  final def foldRight[B](z: => B)(f: (A, => B) => B): B = F.foldRight(self, z)(f)
  final def foldLeft[B](z: B)(f: (B, A) => B): B = F.foldLeft(self, z)(f)
  final def foldR[B](z: => B)(f: A => (=> B) => B): B = F.foldR(self, z)(f)
  final def foldL[B](z: B)(f: B => A => B): B = F.foldL(self, z)(f)
  final def foldR1(f: (A => (=> A) => A)): Option[A] = F.foldR1(self)(f)
  final def foldL1(f: (A => (=> A) => A)): Option[A] = F.foldL1(self)(f)
  final def sumr(implicit A: Monoid[A]): A = F.foldRight(self, A.zero)(A.append)
  final def suml(implicit A: Monoid[A]): A = F.foldLeft(self, A.zero)(A.append(_, _))
  final def toList: List[A] = F.toList(self)
  final def toIndexedSeq: IndexedSeq[A] = F.toIndexedSeq(self)
  final def toSet: Set[A] = F.toSet(self)
  final def toStream: Stream[A] = F.toStream(self)

  ////
}

trait ToFoldableV0 {
  implicit def ToFoldableVUnapply[FA](v: FA)(implicit F0: Unapply[Foldable, FA]) =
    new FoldableV[F0.M,F0.A] { def self = F0(v); implicit def F: Foldable[F0.M] = F0.TC }

}

trait ToFoldableV extends ToFoldableV0 {
  implicit def ToFoldableV[F[_],A](v: F[A])(implicit F0: Foldable[F]) =
    new FoldableV[F,A] { def self = v; implicit def F: Foldable[F] = F0 }

  ////

  ////
}

trait FoldableSyntax[F[_]]  {
  implicit def ToFoldableV[A](v: F[A])(implicit F0: Foldable[F]): FoldableV[F, A] = new FoldableV[F,A] { def self = v; implicit def F: Foldable[F] = F0 }

  ////

  ////
}
