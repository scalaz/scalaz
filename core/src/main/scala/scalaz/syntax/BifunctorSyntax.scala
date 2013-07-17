package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Bifunctor` */
sealed abstract class BifunctorOps[F[_, _],A, B] extends Ops[F[A, B]] {
  implicit def F: Bifunctor[F]
  ////
  import Liskov.<~<

  final def bimap[C, D](f: A => C, g: B => D): F[C, D] = F.bimap(self)(f, g)
  final def :->[D](g: B => D): F[A, D] = F.bimap(self)(a => a, g)
  final def <-:[C](f: A => C): F[C, B] = F.bimap(self)(f, b => b)
  final def <:>[C](f: A => C)(implicit z: B <~< C): F[C, C] = F.bimap(self)(f, z)
  final def umap[C](f: A => C)(implicit ev: F[A, B] =:= F[A, A]): F[C, C] = F.umap(ev(self))(f)
  final def rightMap[D](g: B => D): F[A, D] = F.bimap(self)(a => a, g)
  final def leftMap[C](f: A => C): F[C, B] = F.bimap(self)(f, b => b)
  ////
}

trait ToBifunctorOps0 {
    implicit def ToBifunctorOpsUnapply[FA](v: FA)(implicit F0: Unapply2[Bifunctor, FA]) =
      new BifunctorOps[F0.M,F0.A,F0.B] { def self = F0(v); implicit def F: Bifunctor[F0.M] = F0.TC }
  
}

trait ToBifunctorOps extends ToBifunctorOps0 {
  
  implicit def ToBifunctorOps[F[_, _],A, B](v: F[A, B])(implicit F0: Bifunctor[F]) =
      new BifunctorOps[F,A, B] { def self = v; implicit def F: Bifunctor[F] = F0 }
  

  ////

  ////
}

trait BifunctorSyntax[F[_, _]]  {
  implicit def ToBifunctorOps[A, B](v: F[A, B]): BifunctorOps[F, A, B] = new BifunctorOps[F, A, B] { def self = v; implicit def F: Bifunctor[F] = BifunctorSyntax.this.F }

  def F: Bifunctor[F]
  ////

  ////
}
