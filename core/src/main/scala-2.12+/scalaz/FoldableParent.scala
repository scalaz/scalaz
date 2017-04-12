package scalaz

////
import syntax.foldable.ToFoldableOps
////
trait FoldableParent[F[_]] { self: Foldable[F] =>
  ////

  def sumr1Opt[A](fa: F[A])(implicit A: Semigroup[A]): Option[A] =
    ToFoldableOps(fa)(self).sumr1Opt

  def suml1Opt[A](fa: F[A])(implicit A: Semigroup[A]): Option[A] =
    ToFoldableOps(fa)(self).suml1Opt

  def findMapM[M[_]: Monad, A, B](fa: F[A])(f: A => M[Option[B]]): M[Option[B]] =
    ToFoldableOps(fa)(self).findMapM(f)

  def findLeft[A](fa: F[A])(f: A => Boolean): Option[A] =
    ToFoldableOps(fa)(self).findLeft(f)

  def findRight[A](fa: F[A])(f: A => Boolean): Option[A] =
    ToFoldableOps(fa)(self).findRight(f)

  def filterLength[A](fa: F[A])(f: A => Boolean): Int =
    foldLeft(fa, 0)((b, a) => (if (f(a)) 1 else 0) + b)

  def msuml[G[_], A](fa: F[G[A]])(implicit G: PlusEmpty[G]): G[A] =
    ToFoldableOps(fa)(self).msuml

  def msumlU[GA](fa: F[GA])(implicit G: Unapply[PlusEmpty, GA]): G.M[G.A] =
    msuml[G.M, G.A](G.leibniz.subst[F](fa))(G.TC)

  /**
    * Splits the elements into groups that produce the same result by a function f.
    */
  def splitBy[A, B: Equal](fa: F[A])(f: A => B): IList[(B, NonEmptyList[A])] =
    ToFoldableOps(fa)(self).splitBy(f)

  /**
    * Splits into groups of elements that are transitively dependant by a relation r.
    */
  def splitByRelation[A](fa: F[A])(r: (A, A) => Boolean): IList[NonEmptyList[A]] =
    ToFoldableOps(fa)(self).splitByRelation(r)

  /** Like `fold` but returning `None` if the foldable is empty and `Some` otherwise */
  def fold1Opt[A: Semigroup](fa: F[A]): Option[A] = foldMap1Opt(fa)(a => a)

  ////
}
