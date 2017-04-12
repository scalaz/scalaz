package scalaz

////
////
trait Traverse1Parent[F[_]] { self: Traverse1[F] =>
  ////

  def sequence1U[GA](fga: F[GA])(implicit G: Unapply[Apply, GA]): G.M[F[G.A]] =
    sequence1(G.leibniz.subst(fga))(G.TC)

  ////
}
