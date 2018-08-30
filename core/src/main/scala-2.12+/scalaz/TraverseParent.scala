package scalaz

////
////
trait TraverseParent[F[_]] { self: Traverse[F] =>
  ////

  /** A version of `sequence` where a subsequent monadic join is applied to the inner result */
  def sequenceM[A, G[_]](fgfa: F[G[F[A]]])(implicit G: Applicative[G], F: Bind[F]): G[F[A]] =
    syntax.traverse.ToTraverseOps(fgfa)(self).sequenceM

  ////
}
