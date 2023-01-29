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

  /** The smallest and largest elements of `fa` or None if `fa` is empty */
  def extrema[A: Order](fa: F[A]): Option[(A, A)] =
    extremaBy(fa)(identity)

  /** The smallest and largest values of `f(a)` for each element `a` of `fa` , or None if `fa` is empty */
  def extremaOf[A, B: Order](fa: F[A])(f: A => B): Option[(B, B)] =
    syntax.foldable.ToFoldableOps(fa)(self).extremaOf(f)

  /** The elements (amin, amax) of `fa` which yield the smallest and largest values of `f(a)`, respectively, or None if `fa` is empty */
  def extremaBy[A, B: Order](fa: F[A])(f: A => B): Option[(A, A)] =
    syntax.foldable.ToFoldableOps(fa)(self).extremaBy(f)

  ////
}
