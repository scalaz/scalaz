package scalaz

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
  def apply[F[_, _]](implicit F: BiFunctor[F]): BiFunctor[F] = F

  ////
  /*
  implicit def EitherBiFunctor: BiFunctor[Either] = new BiFunctor[Either] {
    def bimap[A, B, C, D](f: A => C, g: B => D) = {
      case Left(a) => Left(f(a))
      case Right(b) => Right(g(b))
    }
  }

  implicit def LazyEitherBiFunctor: BiFunctor[LazyEither] = new BiFunctor[LazyEither] {
    def bimap[A, B, C, D](f: A => C, g: B => D) =
      _.map(x => g(x)).left.map(x => f(x))
  }

  implicit def EitherTBiFunctor[F[_] : Functor]: BiFunctor[({type λ[α, β] = EitherT[α, F, β]})#λ] = new BiFunctor[({type λ[α, β] = EitherT[α, F, β]})#λ] {
    def bimap[A, B, C, D](f: A => C, g: B => D) =
      _.map(g).left.map(f)
  }

  implicit def LazyEitherTBiFunctor[F[_] : Functor]: BiFunctor[({type λ[α, β] = LazyEitherT[α, F, β]})#λ] = new BiFunctor[({type λ[α, β] = LazyEitherT[α, F, β]})#λ] {
    def bimap[A, B, C, D](f: A => C, g: B => D) =
      _.map(x => g(x)).left.map(x => f(x))
  }

  implicit def ValidationBiFunctor: BiFunctor[Validation] = new BiFunctor[Validation] {
    def bimap[A, B, C, D](f: A => C, g: B => D) = {
      case Failure(a) => Validation.failure(f(a))
      case Success(b) => Validation.success(g(b))
    }
  }

  import java.util.Map.Entry
  import java.util.AbstractMap.SimpleImmutableEntry

  implicit def MapEntryBiFunctor: BiFunctor[Entry] = new BiFunctor[Entry] {
    def bimap[A, B, C, D](f: A => C, g: B => D) =
      k => new SimpleImmutableEntry(f(k.getKey), g(k.getValue))
  }
  */

  ////
}

