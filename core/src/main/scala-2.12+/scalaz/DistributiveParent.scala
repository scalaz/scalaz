package scalaz

////
////
trait DistributiveParent[F[_]] { self: Distributive[F] =>
  ////

  def cotraverse[G[_]:Functor,A,B](gfa: G[F[A]])(f: G[A] => B): F[B] =
    map(cosequence(gfa))(f)


  ////
}
