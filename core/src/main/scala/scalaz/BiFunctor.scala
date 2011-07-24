package scalaz

/**
 * Binary covariant functor.
 *
 * <p>
 * All binary functor instances must satisfy 2 laws:
 * <ol>
 * <li><strong>identity</strong><br/><code>forall a. a == bimap(identity, identity)(a)</code></li>
 * <li><strong>composition</strong><br/><code>forall a f g h i. bimap(f compose g, h compose i)(a) == bimap(f, h)(bimap(g, i)(a))</code></li>
 * </ol>
 * </p>
 */
trait BiFunctor[F[_, _]] {
  def bimap[A, B, C, D](f: A => C, g: B => D): F[A, B] => F[C, D]

  def leftFunctor[X]: Functor[({type λ[α] = F[α, X]})#λ] = new Functor[({type λ[α] = F[α, X]})#λ] {
    def fmap[A, C](f: A => C): F[A, X] => F[C, X] =
      bimap(f, z => z)
  }

  def leftMap[A, B, C](f: A => C): F[A, B] => F[C, B] =
    bimap(f, z => z)

  def rightFunctor[X]: Functor[({type λ[α] = F[X, α]})#λ] = new Functor[({type λ[α] = F[X, α]})#λ] {
    def fmap[B, D](g: B => D): F[X, B] => F[X, D] =
      bimap(z => z, g)
  }

  def rightMap[A, B, D](g: B => D): F[A, B] => F[A, D] =
    bimap(z => z, g)

  def umap[A, B](f: A => B): F[A, A] => F[B, B] =
    bimap(f, f)
}

object BiFunctor {
  type Bifunctor[F[_, _]] =
  BiFunctor[F]

  implicit def Tuple2BiFunctor: BiFunctor[Tuple2] = new BiFunctor[Tuple2] {
    def bimap[A, B, C, D](f: A => C, g: B => D) =
      k => (f(k._1), g(k._2))
  }

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

}
