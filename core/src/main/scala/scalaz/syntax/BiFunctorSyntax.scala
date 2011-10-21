package scalaz
package syntax


/** Wraps a value `self` and provides methods related to `BiFunctor` */
trait BiFunctorV[F[_, _],A, B] extends SyntaxV[F[A, B]] {
  ////
  def :->[D](g: B => D)(implicit b: BiFunctor[F]): F[A, D] = b.bimap(self)(a => a, g)

  def <-:[C](f: A => C)(implicit b: BiFunctor[F]): F[C, B] = b.bimap(self)(f, b => b)

  ////
}

trait ToBiFunctorSyntax  {
  implicit def ToBiFunctorV[F[_, _],A, B](v: F[A, B]) =
    new BiFunctorV[F,A, B] { def self = v }

  ////

  ////
}

trait BiFunctorSyntax[F[_, _]]  {
  implicit def ToBiFunctorV[A, B](v: F[A, B]): BiFunctorV[F, A, B] = new BiFunctorV[F, A, B] { def self = v }

  ////

  ////
}
