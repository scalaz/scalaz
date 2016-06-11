package scalaz

////
/**
 * A type giving rise to two unrelated [[scalaz.Functor]]s.
 */
////
trait Bifunctor[F[_, _]]  { self =>
  ////

  /** `map` over both type parameters. */
  def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D]

  /**The composition of Bifunctors `F` and `G`, `[x,y]F[G[x,y],G[x,y]]`, is a Bifunctor */
  def compose[G[_, _]](implicit G0: Bifunctor[G]): Bifunctor[λ[(α, β) => F[G[α, β], G[α, β]]]] =
    new CompositionBifunctor[F, G] {
      implicit def F = self
      implicit def G = G0
    }

  /**The product of Bifunctors `F` and `G`, `[x,y](F[x,y], G[x,y])`, is a Bifunctor */
  def product[G[_, _]](implicit G0: Bifunctor[G]): Bifunctor[λ[(α, β) => (F[α, β], G[α, β])]] =
    new ProductBifunctor[F, G] {
      implicit def F = self
      implicit def G = G0
    }

  /** Extract the Functor on the first param. */
  def leftFunctor[X]: Functor[F[?, X]] =
    new LeftFunctor[F, X] {val F = self}

  def leftMap[A, B, C](fab: F[A, B])(f: A => C): F[C, B] =
    bimap(fab)(f, z => z)

  /** Extract the Functor on the second param. */
  def rightFunctor[X]: Functor[F[X, ?]] =
    new RightFunctor[F, X] {val F = self}

  /** Unify the functor over both params. */
  def uFunctor: Functor[λ[α => F[α, α]]] =
    new UFunctor[F] {val F = self}

  def rightMap[A, B, D](fab: F[A, B])(g: B => D): F[A, D] =
    bimap(fab)(z => z, g)

  def umap[A, B](faa: F[A, A])(f: A => B): F[B, B] =
    bimap(faa)(f, f)

  /** Embed two Functors , one on each side */
  def embed[G[_],H[_]](implicit G0: Functor[G], H0: Functor[H]): Bifunctor[λ[(α, β) => F[G[α],H[β]]]] =
    new CompositionBifunctorFunctors[F,G,H] {
      def F = self
      def G = G0
      def H = H0
    }

  /** Embed one Functor to the left */
  def embedLeft[G[_]](implicit G0: Functor[G]): Bifunctor[λ[(α, β) => F[G[α],β]]] =
    embed[G,Id.Id]

  /** Embed one Functor to the right */
  def embedRight[H[_]](implicit H0: Functor[H]): Bifunctor[λ[(α, β) => F[α,H[β]]]] =
    embed[Id.Id,H]

  ////
  val bifunctorSyntax = new scalaz.syntax.BifunctorSyntax[F] { def F = Bifunctor.this }
}

object Bifunctor {
  @inline def apply[F[_, _]](implicit F: Bifunctor[F]): Bifunctor[F] = F

  ////

  ////
}
