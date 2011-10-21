package scalaz

trait BiTraverse[F[_, _]] extends BiFunctor[F] {
  self =>
  ////
  def bitraverse[G[_] : Applicative, A, B, C, D](fab: F[A, B])(f: A => G[C], g: B => G[D]): G[F[C, D]]

  // derived functions

  def bimap[A, B, C, D](fab: F[A, B])(f: (A) => C, g: (B) => D): F[C, D] = {
    import Ident.id
    bitraverse[Id, A, B, C, D](fab)(f, g)
  }
  
  def bisequence[G[_] : Applicative, A, B](x: F[G[A], G[B]]): G[F[A, B]] = bitraverse(x)(fa => fa, fb => fb)

  ////
  val biTraverseSyntax = new scalaz.syntax.BiTraverseSyntax[F] {}
}

object BiTraverse {
  def apply[F[_, _]](implicit F: BiTraverse[F]): BiTraverse[F] = F

  ////

  /*
  TODO
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
   */
  ////
}

