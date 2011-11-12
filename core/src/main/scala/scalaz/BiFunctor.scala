package scalaz

////
/**
 *
 */
////
trait BiFunctor[F[_, _]]  { self =>
  ////
  def bimap[A, B, C, D](fab: F[A, B])(f: A => C, g: B => D): F[C, D]

  def leftFunctor[X]: Functor[({type λ[α] = F[α, X]})#λ] = new Functor[({type λ[α] = F[α, X]})#λ] {
    def map[A, C](fax: F[A, X])(f: A => C): F[C, X] = bimap(fax)(f, z => z)
  }

  def leftMap[A, B, C](fab: F[A, B])(f: A => C): F[C, B] = bimap(fab)(f, z => z)

  def rightFunctor[X]: Functor[({type λ[α] = F[X, α]})#λ] = new Functor[({type λ[α] = F[X, α]})#λ] {
    def map[B, D](fab: F[X, B])(g: B => D): F[X, D] = bimap(fab)(z => z, g)
  }

  def rightMap[A, B, D](fab: F[A, B])(g: B => D): F[A, D] =
    bimap(fab)(z => z, g)

  def umap[A, B](faa: F[A, A])(f: A => B): F[B, B] =
    bimap(faa)(f, f)
  ////
  val biFunctorSyntax = new scalaz.syntax.BiFunctorSyntax[F] {}
}

object BiFunctor {
  @inline def apply[F[_, _]](implicit F: BiFunctor[F]): BiFunctor[F] = F

  ////

  ////
}

