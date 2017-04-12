package scalaz

////
import syntax.traverse.ToTraverseOps
////
trait TraverseParent[F[_]] { self: Traverse[F] =>
  ////

  def indexed[A](fa: F[A]): F[(Int, A)] =
    ToTraverseOps(fa)(self).indexed

  ////
}
