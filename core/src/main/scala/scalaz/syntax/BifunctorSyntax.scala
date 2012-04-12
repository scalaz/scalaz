package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `Bifunctor` */
trait BifunctorV[F[_, _],A, B] extends SyntaxV[F[A, B]] {
  implicit def F: Bifunctor[F]
  ////
  import Liskov.<~<

  final def bimap[C, D](f: A => C, g: B => D): F[C, D] = F.bimap(self)(f, g)
  final def :->[D](g: B => D): F[A, D] = F.bimap(self)(a => a, g)
  final def <-:[C](f: A => C): F[C, B] = F.bimap(self)(f, b => b)
  final def <:>[C](f: A => C)(implicit z: B <~< C): F[C, C] = F.bimap(self)(f, z)

  ////
}

trait ToBifunctorV0 {
    implicit def ToBifunctorVUnapply[FA](v: FA)(implicit F0: Unapply2[Bifunctor, FA]) =
      new BifunctorV[F0.M,F0.A,F0.B] { def self = F0(v); implicit def F: Bifunctor[F0.M] = F0.TC }
  
}

trait ToBifunctorV extends ToBifunctorV0 {
  
  implicit def ToBifunctorV[F[_, _],A, B](v: F[A, B])(implicit F0: Bifunctor[F]) =
      new BifunctorV[F,A, B] { def self = v; implicit def F: Bifunctor[F] = F0 }
  

  ////

  ////
}

trait BifunctorSyntax[F[_, _]]  {
  implicit def ToBifunctorV[A, B](v: F[A, B])(implicit F0: Bifunctor[F]): BifunctorV[F, A, B] = new BifunctorV[F, A, B] { def self = v; implicit def F: Bifunctor[F] = F0 }

  ////

  ////
}
