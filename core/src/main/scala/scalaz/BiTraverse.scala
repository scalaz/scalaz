package scalaz


trait BiTraverse[T[_, _]] {
  def bitraverse[F[_] : Applicative, A, B, C, D](f: A => F[B], g: C => F[D]): T[A, C] => F[T[B, D]]

  def bitrav[F[_] : Applicative, A, B, C, D](a: T[A, C], f: A => F[B], g: C => F[D]): F[T[B, D]] = {
    val z = bitraverse(f, g)
    z(a)
  }

  def biFunctor: BiFunctor[T] = new BiFunctor[T] {
    def bimap[A, B, C, D](f: A => C, g: B => D) =
      bitrav[Identity, A, C, B, D](_, a => Identity.id(f(a)), b => Identity.id(g(b))).value
  }

  def bimap[A, B, C, D](f: A => C, g: B => D): T[A, B] => T[C, D] =
    biFunctor.bimap(f, g)

  def bisequence[F[_]: Applicative, A, B](x: T[F[A], F[B]]): F[T[A, B]] =
    bitrav(x, identity[F[A]], identity[F[B]])

  def leftFunctor[X]: Functor[({type λ[α] = T[α, X]})#λ] = new Functor[({type λ[α] = T[α, X]})#λ] {
    def fmap[A, C](f: A => C): T[A, X] => T[C, X] =
      bimap(f, z => z)
  }

  def leftMap[A, B, C](f: A => C): T[A, B] => T[C, B] =
    bimap(f, z => z)

  def rightFunctor[X]: Functor[({type λ[α] = T[X, α]})#λ] = new Functor[({type λ[α] = T[X, α]})#λ] {
    def fmap[B, D](g: B => D): T[X, B] => T[X, D] =
      bimap(z => z, g)
  }

  def rightMap[A, B, D](g: B => D): T[A, B] => T[A, D] =
    bimap(z => z, g)

  def umap[A, B](f: A => B): T[A, A] => T[B, B] =
    bimap(f, f)

}

object BiTraverse extends BiTraverses

trait BiTraverses {
  type Bitraverse[T[_, _]] =
  BiTraverse[T]


  implicit def Tuple2BiTraverse: BiTraverse[Tuple2] = new BiTraverse[Tuple2] {
    def bitraverse[F[_] : Applicative, A, B, C, D](f: A => F[B], g: C => F[D]): ((A, C)) => F[(B, D)] = {
      case (a, c) =>
        implicitly[Applicative[F]].liftA2((b: B) => (d: D) => (b, d))(f(a))(g(c))
    }
  }

  implicit def EitherBiTraverse: BiTraverse[Either] = new BiTraverse[Either] {
    def bitraverse[F[_] : Applicative, A, B, C, D](f: A => F[B], g: C => F[D]) = {
      case Left(a) => implicitly[Applicative[F]].fmap((b: B) => Left(b): Either[B, D])(f(a))
      case Right(b) => implicitly[Applicative[F]].fmap((d: D) => Right(d): Either[B, D])(g(b))
    }
  }

  implicit def LazyEitherBiTraverse: BiTraverse[LazyEither] = new BiTraverse[LazyEither] {
    def bitraverse[F[_] : Applicative, A, B, C, D](f: A => F[B], g: C => F[D]) =
      _.fold(
        a => implicitly[Applicative[F]].fmap((b: B) => LazyEither.lazyLeft[D](b))(f(a))
      , b => implicitly[Applicative[F]].fmap((d: D) => LazyEither.lazyRight[B](d))(g(b))
      )
  }

  implicit def EitherTBiTraverse[F[_] : Traverse]: BiTraverse[({type λ[α, β] = EitherT[α, F, β]})#λ] = new BiTraverse[({type λ[α, β] = EitherT[α, F, β]})#λ] {
    def bitraverse[G[_] : Applicative, A, B, C, D](f: A => G[B], g: C => G[D]) =
      z =>
        implicitly[Applicative[G]].fmap(EitherT.eitherT(_: F[Either[B, D]]))(implicitly[Traverse[F]].trav(z.runT, implicitly[BiTraverse[Either]].bitraverse(f, g)))
  }

  implicit def LazyEitherTBiTraverse[F[_] : Traverse]: BiTraverse[({type λ[α, β] = LazyEitherT[α, F, β]})#λ] = new BiTraverse[({type λ[α, β] = LazyEitherT[α, F, β]})#λ] {
    def bitraverse[G[_] : Applicative, A, B, C, D](f: A => G[B], g: C => G[D]) =
      z =>
        implicitly[Applicative[G]].fmap(LazyEitherT.lazyEitherT(_: F[LazyEither[B, D]]))(implicitly[Traverse[F]].trav(z.runT, implicitly[BiTraverse[LazyEither]].bitraverse(f, g)))
  }

  implicit def ValidationBiTraverse: BiTraverse[Validation] = new BiTraverse[Validation] {
    def bitraverse[F[_] : Applicative, A, B, C, D](f: A => F[B], g: C => F[D]) =
      _.fold(
        a => implicitly[Applicative[F]].fmap((b: B) => Validation.failure[D](b))(f(a))
      , b => implicitly[Applicative[F]].fmap((d: D) => Validation.success[B](d))(g(b))
      )
  }

  import java.util.Map.Entry
  import java.util.AbstractMap.SimpleImmutableEntry

  implicit def MapEntryBiTraverse: BiTraverse[Entry] = new BiTraverse[Entry] {
    def bitraverse[F[_] : Applicative, A, B, C, D](f: A => F[B], g: C => F[D]) = v => {
      implicitly[Applicative[F]].liftA2((b: B) => (d: D) => new SimpleImmutableEntry(b, d): Entry[B, D])(f(v.getKey))(g(v.getValue))
    }
  }

}