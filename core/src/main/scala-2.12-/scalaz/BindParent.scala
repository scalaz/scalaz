package scalaz

////
////
trait BindParent[F[_]] { self: Bind[F] =>
  ////
  def forever[A, B](fa: F[A]): F[B]
  ////
}
