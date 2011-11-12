package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Foldable` */
trait FoldableV[F[_],A] extends SyntaxV[F[A]] {
  implicit def F: Foldable[F]
  ////
  final def foldMap[B](f: A => B)(implicit B: Monoid[B]): B = F.foldMap(self)(f)
  final def foldMapIdentity[B](implicit B: Monoid[A]): A = F.foldMapIdentity(self)
  final def toList: List[A] = F.toList(self)
  final def toIndexedSeq(fa: F[A]): IndexedSeq[A] = F.toIndexedSeq(self)
  final def toSet(fa: F[A]): Set[A] = F.toSet(self)
  final def toStream(fa: F[A]): Stream[A] = F.toStream(self)
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
