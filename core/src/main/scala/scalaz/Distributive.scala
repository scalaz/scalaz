package scalaz

////
/** Dual of [[scalaz.Traverse]].  To transform `F[G[B]]` to `G[F[B]]`,
  * you may use `Traverse[F]` and `Applicative[G]`, but alternatively
  * `Functor[F]` and `Distributive[G]`, which permits greater sharing
  * and nonstrictness.
  */
////
trait Distributive[F[_]] extends Functor[F] { self =>
  ////

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

  ////
  val distributiveSyntax = new scalaz.syntax.DistributiveSyntax[F] { def F = Distributive.this }
}

object Distributive {
  @inline def apply[F[_]](implicit F: Distributive[F]): Distributive[F] = F

  import Isomorphism._

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: Distributive[G]): Distributive[F] =
    new IsomorphismDistributive[F, G] {
      override def G: Distributive[G] = E
      override def iso: F <~> G = D
    }

  ////

  // Distributive is the dual of Traverse.
  type Cotraverse[F[_]] =
  Distributive[F]

  ////
}

trait IsomorphismDistributive[F[_], G[_]] extends Distributive[F] with IsomorphismFunctor[F, G]{
  implicit def G: Distributive[G]
  ////

  def distributeImpl[H[_]: Functor, A, B](a: H[A])(f: A => F[B]): F[H[B]] =
    iso.from(G.distributeImpl(a)(x => iso.to(f(x))))

  ////
}
