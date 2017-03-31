package scalaz

////
////
trait Traverse1Parent[F[_]] { self: Traverse1[F] =>
  ////

  def sequence1U[GA](fga: F[GA])(implicit G: Unapply[Apply, GA]): G.M[F[G.A]] =
    sequence1(G.leibniz.subst(fga))(G.TC)

  def traverse1U[A, GB](fa: F[A])(f: A => GB)(implicit G: Unapply[Apply, GB]): G.M[F[G.A]] =
    traverse1(fa)(G.leibniz.onF(f))(G.TC)

  ////
}
