package scalaz

////
////
trait FoldableParent[F[_]] { self: Foldable[F] =>
  ////

  def filterLength[A](fa: F[A])(f: A => Boolean): Int =
    foldLeft(fa, 0)((b, a) => (if (f(a)) 1 else 0) + b)

  def msumlU[GA](fa: F[GA])(implicit G: Unapply[PlusEmpty, GA]): G.M[G.A] =
    msuml[G.M, G.A](G.leibniz.subst[F](fa))(G.TC)

  /**
    * Splits the elements into groups that produce the same result by a function f.
    */
  def splitBy[A, B: Equal](fa: F[A])(f: A => B): IList[(B, NonEmptyList[A])] =
    syntax.foldable.ToFoldableOps(fa)(self).splitBy(f)

  /**
    * Splits into groups of elements that are transitively dependant by a relation r.
    */
  def splitByRelation[A](fa: F[A])(r: (A, A) => Boolean): IList[NonEmptyList[A]] =
    syntax.foldable.ToFoldableOps(fa)(self).splitByRelation(r)

  /** Like `fold` but returning `None` if the foldable is empty and `Some` otherwise */
  def fold1Opt[A: Semigroup](fa: F[A]): Option[A] = foldMap1Opt(fa)(a => a)

  def distinctBy[A, B: Equal](fa: F[A])(f: A => B): IList[A] =
    distinctE(fa)(Equal.equalBy(f))
  ////
}
