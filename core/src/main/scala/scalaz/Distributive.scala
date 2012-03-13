package scalaz

trait Distributive[F[_]] extends Functor[F] {
  def distributeImpl[G[_]:Functor,A,B](fa: G[A])(f: A => F[B]): F[G[B]]


}

object Distributive extends DistributiveFunctions {
  @inline def apply[F[_]](implicit F: Distributive[F]): Distributive[F] = F

}
trait DistributiveFunctions {
  // Distributive is the dual of Traverse.
  type Cotraverse[F[_]] =
  Distributive[F]
}
