package scalaz

////
/**
 *
 */
////
trait BiTraverse[F[_, _]] extends BiFunctor[F] { self =>
  ////
  def bitraverse[G[_] : Applicative, A, B, C, D](fab: F[A, B])(f: A => G[C], g: B => G[D]): G[F[C, D]]

  // derived functions

  def bitraverseF[G[_] : Applicative, A, B, C, D](f: A => G[C], g: B => G[D]): F[A, B] => G[F[C, D]] =
    bitraverse(_)(f, g)

  def bimap[A, B, C, D](fab: F[A, B])(f: (A) => C, g: (B) => D): F[C, D] = {
    bitraverse[Id, A, B, C, D](fab)(f, g)
  }
  
  def bisequence[G[_] : Applicative, A, B](x: F[G[A], G[B]]): G[F[A, B]] = bitraverse(x)(fa => fa, fb => fb)

  ////
  val biTraverseSyntax = new scalaz.syntax.BiTraverseSyntax[F] {}
}

object BiTraverse {
  @inline def apply[F[_, _]](implicit F: BiTraverse[F]): BiTraverse[F] = F

  ////

  ////
}

