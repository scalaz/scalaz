package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Foldable` */
sealed abstract class FoldableOps[F[_],A] extends Ops[F[A]] {
  implicit def F: Foldable[F]
  ////
  import collection.generic.CanBuildFrom
  import collection.immutable.IndexedSeq

  final def foldMap[B: Monoid](f: A => B = (a: A) => a): B = F.foldMap(self)(f)
  final def foldMap1Opt[B: Semigroup](f: A => B = (a: A) => a): Option[B] = F.foldMap1Opt(self)(f)
  final def foldRight[B](z: => B)(f: (A, => B) => B): B = F.foldRight(self, z)(f)
  final def foldRight1Opt(f: (A, => A) => A): Option[A] = F.foldRight1Opt(self)(f)
  final def foldLeft[B](z: B)(f: (B, A) => B): B = F.foldLeft(self, z)(f)
  final def foldLeft1Opt(f: (A, A) => A): Option[A] = F.foldLeft1Opt(self)(f)
  final def foldRightM[G[_], B](z: => B)(f: (A, => B) => G[B])(implicit M: Monad[G]): G[B] = F.foldRightM(self, z)(f)
  final def foldLeftM[G[_], B](z: B)(f: (B, A) => G[B])(implicit M: Monad[G]): G[B] = F.foldLeftM(self, z)(f)
  final def fold(implicit A: Monoid[A]): A = F.fold(self)(A)
  final def foldr[B](z: => B)(f: A => (=> B) => B): B = F.foldr(self, z)(f)
  final def foldr1Opt(f: A => (=> A) => A): Option[A] = F.foldr1Opt(self)(f)
  final def foldl[B](z: B)(f: B => A => B): B = F.foldl(self, z)(f)
  final def foldl1Opt(f: A => A => A): Option[A] = F.foldl1Opt(self)(f)
  final def foldrM[G[_], B](z: => B)(f: A => ( => B) => G[B])(implicit M: Monad[G]): G[B] = F.foldrM(self, z)(f)
  final def foldlM[G[_], B](z: B)(f: B => A => G[B])(implicit M: Monad[G]): G[B] = F.foldlM(self, z)(f)
  final def length: Int = F.length(self)
  final def index(n: Int): Option[A] = F.index(self, n)
  final def indexOr(default: => A, n: Int): A = F.indexOr(self, default, n)
  final def sumr(implicit A: Monoid[A]): A = F.foldRight(self, A.zero)(A.append)
  final def suml(implicit A: Monoid[A]): A = F.foldLeft(self, A.zero)(A.append(_, _))
  final def toList: List[A] = F.toList(self)
  final def toIndexedSeq: IndexedSeq[A] = F.toIndexedSeq(self)
  final def toSet: Set[A] = F.toSet(self)
  final def toStream: Stream[A] = F.toStream(self)
  final def to[G[_]](implicit c: CanBuildFrom[Nothing, A, G[A]]) = F.to[A, G](self)
  final def all(p: A => Boolean): Boolean = F.all(self)(p)
  final def ∀(p: A => Boolean): Boolean = F.all(self)(p)
  final def allM[G[_]: Monad](p: A => G[Boolean]): G[Boolean] = F.allM(self)(p)
  final def anyM[G[_]: Monad](p: A => G[Boolean]): G[Boolean] = F.anyM(self)(p)
  final def any(p: A => Boolean): Boolean = F.any(self)(p)
  final def ∃(p: A => Boolean): Boolean = F.any(self)(p)
  final def count: Int = F.count(self)
  final def maximum(implicit A: Order[A]): Option[A] = F.maximum(self)
  final def maximumOf[B: Order](f: A => B): Option[B] = F.maximumOf(self)(f)
  final def maximumBy[B: Order](f: A => B): Option[A] = F.maximumBy(self)(f)
  final def minimum(implicit A: Order[A]): Option[A] = F.minimum(self)
  final def minimumOf[B: Order](f: A => B): Option[B] = F.minimumOf(self)(f)
  final def minimumBy[B: Order](f: A => B): Option[A] = F.minimumBy(self)(f)
  final def longDigits(implicit d: A <:< Digit): Long = F.longDigits(self)
  final def empty: Boolean = F.empty(self)
  final def element(a: A)(implicit A: Equal[A]): Boolean = F.element(self, a)
  final def splitWith(p: A => Boolean): List[List[A]] = F.splitWith(self)(p)
  final def selectSplit(p: A => Boolean): List[List[A]] = F.selectSplit(self)(p)
  final def collapse[X[_]](implicit A: ApplicativePlus[X]): X[A] = F.collapse(self)
  final def concatenate(implicit A: Monoid[A]): A = F.fold(self)
  final def intercalate(a: A)(implicit A: Monoid[A]): A = F.intercalate(self, a)
  final def traverse_[M[_]:Applicative](f: A => M[Unit]): M[Unit] = F.traverse_(self)(f)
  final def traverseS_[S, B](f: A => State[S, B]): State[S, Unit] = F.traverseS_(self)(f)

  ////
}

trait ToFoldableOps0 {
  implicit def ToFoldableOpsUnapply[FA](v: FA)(implicit F0: Unapply[Foldable, FA]) =
    new FoldableOps[F0.M,F0.A] { def self = F0(v); implicit def F: Foldable[F0.M] = F0.TC }

}

trait ToFoldableOps extends ToFoldableOps0 {
  implicit def ToFoldableOps[F[_],A](v: F[A])(implicit F0: Foldable[F]) =
    new FoldableOps[F,A] { def self = v; implicit def F: Foldable[F] = F0 }

  ////

  ////
}

trait FoldableSyntax[F[_]]  {
  implicit def ToFoldableOps[A](v: F[A]): FoldableOps[F, A] = new FoldableOps[F,A] { def self = v; implicit def F: Foldable[F] = FoldableSyntax.this.F }

  def F: Foldable[F]
  ////

  ////
}
