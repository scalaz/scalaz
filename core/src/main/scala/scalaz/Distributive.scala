package scalaz

/** Dual of [[scalaz.Traverse]].  To transform `F[G[B]]` to `G[F[B]]`,
  * you may use `Traverse[F]` and `Applicative[G]`, but alternatively
  * `Functor[F]` and `Distributive[G]`, which permits greater sharing
  * and nonstrictness.
  */
trait Distributive[F[_]] extends Functor[F] { self =>
  def distributeImpl[G[_]:Functor,A,B](fa: G[A])(f: A => F[B]): F[G[B]]


  /**The composition of Distributives `F` and `G`, `[x]F[G[x]]`, is a Distributive */
  def compose[G[_]](implicit G0: Distributive[G]): Distributive[λ[α => F[G[α]]]] =
    new CompositionDistributive[F, G] {
      implicit def F = self
      implicit def G = G0
    }

  /**The product of Distributives `F` and `G`, `[x](F[x], G[x]])`, is a Distributive */
  def product[G[_]](implicit G0: Distributive[G]): Distributive[λ[α => (F[α], G[α])]] =
    new ProductDistributive[F, G] {
      implicit def F = self
      implicit def G = G0
    }

  class Distribution[G[_]](implicit G: Functor[G]) {
    def run[A,B](fa: G[A])(f: A => F[B]): F[G[B]] = distributeImpl(fa)(f)
  }

  def distribution[G[_]:Functor]: Distribution[G] =
    new Distribution[G]

  def distribute[G[_]:Functor,A,B](fa: G[A])(f: A => F[B]): F[G[B]] =
    distribution[G].run(fa)(f)

  def cosequence[G[_]:Functor,A](fa: G[F[A]]): F[G[A]] =
    distributeImpl(fa)(x => x)

}

object Distributive {
  @inline def apply[F[_]](implicit F: Distributive[F]): Distributive[F] = F

  // Distributive is the dual of Traverse.
  type Cotraverse[F[_]] =
  Distributive[F]
}
