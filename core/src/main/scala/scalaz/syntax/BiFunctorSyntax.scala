package scalaz
package syntax

/** Wraps a value `self` and provides methods related to `BiFunctor` */
trait BiFunctorV[F[_, _],A, B] extends SyntaxV[F[A, B]] {
  implicit def F: BiFunctor[F]
  ////
  import Liskov.<~<

  final def bimap[C, D](f: A => C, g: B => D): F[C, D] = F.bimap(self)(f, g)
  final def :->[D](g: B => D): F[A, D] = F.bimap(self)(a => a, g)
  final def <-:[C](f: A => C): F[C, B] = F.bimap(self)(f, b => b)
  final def <:>[C](f: A => C)(implicit z: B <~< C): F[C, C] = F.bimap(self)(f, z)

  ////
}

trait ToBiFunctorV  {
    implicit def ToBiFunctorV[FA](v: FA)(implicit F0: Unapply2[BiFunctor, FA]) =
      new BiFunctorV[F0.M,F0.A,F0.B] { def self = F0(v); implicit def F: BiFunctor[F0.M] = F0.TC }
  

  ////

  ////
}

trait BiFunctorSyntax[F[_, _]]  {
  implicit def ToBiFunctorV[A, B](v: F[A, B])(implicit F0: BiFunctor[F]): BiFunctorV[F, A, B] = new BiFunctorV[F, A, B] { def self = v; implicit def F: BiFunctor[F] = F0 }

  ////

  ////
}
