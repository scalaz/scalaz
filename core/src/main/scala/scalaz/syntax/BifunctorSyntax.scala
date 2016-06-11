package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Bifunctor` */
final class BifunctorOps[F[_, _],A, B] private[syntax](val self: F[A, B])(implicit val F: Bifunctor[F]) extends Ops[F[A, B]] {
  ////
  import Liskov.<~<

  final def bimap[C, D](f: A => C, g: B => D): F[C, D] = F.bimap(self)(f, g)
  final def :->[D](g: B => D): F[A, D] = F.bimap(self)(a => a, g)
  final def <-:[C](f: A => C): F[C, B] = F.bimap(self)(f, b => b)
  final def <:>[C](f: A => C)(implicit z: B <~< C): F[C, C] = F.bimap(self)(f, z)
  final def umap[C](f: A => C)(implicit ev: F[A, B] =:= F[A, A]): F[C, C] = F.umap(ev(self))(f)
  final def rightMap[D](g: B => D): F[A, D] = F.bimap(self)(a => a, g)
  final def leftMap[C](f: A => C): F[C, B] = F.bimap(self)(f, b => b)
  final def rightAs[C](c: => C): F[A, C] = F.bimap(self)(a => a, _ => c)
  final def leftAs[C](c: => C): F[C, B] = F.bimap(self)(_ => c, b => b)
  final def widen[C >: A, D >: B]: F[C, D] = F.bimap(self)(identity[C], identity[D])
  ////
}

sealed trait ToBifunctorOps0 {
  implicit def ToBifunctorOpsUnapply[FA](v: FA)(implicit F0: Unapply2[Bifunctor, FA]) =
    new BifunctorOps[F0.M,F0.A,F0.B](F0(v))(F0.TC)

}

trait ToBifunctorOps extends ToBifunctorOps0 {

  implicit def ToBifunctorOps[F[_, _],A, B](v: F[A, B])(implicit F0: Bifunctor[F]) =
    new BifunctorOps[F,A, B](v)


  implicit def ToBifunctorVFromKleisliLike[G[_], F[G[_], _, _],A, B](v: F[G, A, B])(implicit F0: Bifunctor[F[G, ?, ?]]) =
    new BifunctorOps[F[G, ?, ?], A, B](v)(F0)

  ////

  ////
}

trait BifunctorSyntax[F[_, _]]  {
  implicit def ToBifunctorOps[A, B](v: F[A, B]): BifunctorOps[F, A, B] = new BifunctorOps[F, A, B](v)(BifunctorSyntax.this.F)

  def F: Bifunctor[F]
  ////

  ////
}
