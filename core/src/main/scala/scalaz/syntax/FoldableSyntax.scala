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
  ////
}

trait ToFoldableV  {
  implicit def ToFoldableV[F[_],A](v: F[A])(implicit F0: Foldable[F]) =
    new FoldableV[F,A] { def self = v; implicit def F: Foldable[F] = F0 }
  implicit def ToFoldableVFromBin[F[_, _], X, A](v: F[X, A])(implicit F0: Foldable[({type f[a] = F[X, a]})#f]) =
    new FoldableV[({type f[a] = F[X, a]})#f,A] { def self = v; implicit def F: Foldable[({type f[a] = F[X, a]})#f] = F0 }
  implicit def ToFoldableVFromBinT[F[_, _[_], _], G[_], X, A](v: F[X, G, A])(implicit F0: Foldable[({type f[a] = F[X, G, a]})#f]) =
    new FoldableV[({type f[a] = F[X, G, a]})#f,A] { def self = v; implicit def F: Foldable[({type f[a] = F[X, G, a]})#f] = F0 }
  implicit def ToFoldableVFromBinTId[F[_, _[_], _], X, A](v: F[X, Id, A])(implicit F0: Foldable[({type f[a] = F[X, Id, a]})#f]) =
    new FoldableV[({type f[a] = F[X, Id, a]})#f,A] { def self = v; implicit def F: Foldable[({type f[a] = F[X, Id, a]})#f] = F0 }

  ////

  ////
}

trait FoldableSyntax[F[_]]  {
  implicit def ToFoldableV[A](v: F[A])(implicit F0: Foldable[F]): FoldableV[F, A] = new FoldableV[F,A] { def self = v; implicit def F: Foldable[F] = F0 }

  ////

  ////
}
