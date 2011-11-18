package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Foldable` */
trait FoldableV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: Foldable[F]
  ////
  final def foldMap[B: Monoid](f: A => B = (a: A) => a): B = F.foldMap(self)(f)
  final def toList: List[A] = F.toList(self)
  final def toIndexedSeq: IndexedSeq[A] = F.toIndexedSeq(self)
  final def toSet: Set[A] = F.toSet(self)
  final def toStream: Stream[A] = F.toStream(self)
  ////
}

trait ToFoldableV  {
  implicit def ToFoldableV[FA](v: FA)(implicit F0: Unapply[Foldable, FA]) =
    new FoldableV[F0.M,F0.A] { def self = F0(v); implicit def F: Foldable[F0.M] = F0.TC }

  ////

  ////
}

trait FoldableSyntax[F[_]]  {
  implicit def ToFoldableV[A](v: F[A])(implicit F0: Foldable[F]): FoldableV[F, A] = new FoldableV[F,A] { def self = v; implicit def F: Foldable[F] = F0 }

  ////

  ////
}
