package scalaz

////
/**
 * A type giving rise to two unrelated [[scalaz.Functor]]s.
 */
////
trait Bifunctor[F[_, _]]  { self =>
  /** `map` over both type parameters. */
  def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D]

  /**The composition of Bifunctors `F` and `G`, `[x,y]F[G[x,y],G[x,y]]`, is a Bifunctor */
  def compose[G[_, _]](implicit G0: Bifunctor[G]): Bifunctor[({type λ[α, β]=F[G[α, β], G[α, β]]})#λ] = new CompositionBifunctor[F, G] {
    implicit def F = self

    implicit def G = G0
  }

  /**The product of Bifunctors `F` and `G`, `[x,y]F[G[x,y],G[x,y]]`, is a Bifunctor */
  def product[G[_, _]](implicit G0: Bifunctor[G]): Bifunctor[({type λ[α, β]=(F[α, β], G[α, β])})#λ] = new ProductBifunctor[F, G] {
    implicit def F = self

    implicit def G = G0
  }

  /** Extract the Functor on the first param. */
  def leftFunctor[X]: Functor[({type λ[α] = F[α, X]})#λ] = new Functor[({type λ[α] = F[α, X]})#λ] {
    def map[A, C](fax: F[A, X])(f: A => C): F[C, X] = bimap(fax)(f, z => z)
  }

  def leftMap[A, B, C](fab: F[A, B])(f: A => C): F[C, B] = bimap(fab)(f, z => z)

  /** Extract the Functor on the second param. */
  def rightFunctor[X]: Functor[({type λ[α] = F[X, α]})#λ] = new Functor[({type λ[α] = F[X, α]})#λ] {
    def map[B, D](fab: F[X, B])(g: B => D): F[X, D] = bimap(fab)(z => z, g)
  }

  def rightMap[A, B, D](fab: F[A, B])(g: B => D): F[A, D] =
    bimap(fab)(z => z, g)

  def umap[A, B](faa: F[A, A])(f: A => B): F[B, B] =
    bimap(faa)(f, f)
  ////
  val bifunctorSyntax = new scalaz.syntax.BifunctorSyntax[F] { def F = Bifunctor.this }
}

object Bifunctor {
  @inline def apply[F[_, _]](implicit F: Bifunctor[F]): Bifunctor[F] = F

  ////

  ////
}
