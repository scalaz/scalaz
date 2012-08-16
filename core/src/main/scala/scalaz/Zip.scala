package scalaz

////
import PLens._


/**
 *
 */
////
trait Zip[F[_]]  { self =>
  ////
  def zip[A, B](a: => F[A], b: => F[B]): F[(A, B)]

  // derived functions

  /**The composition of Zip `F` and `G`, `[x]F[G[x]]`, is a Zip (if F is a Functor) */
  def compose[G[_]](implicit T0: Functor[F], G0: Zip[G]): Zip[({type λ[α] = F[G[α]]})#λ] = new CompositionZip[F, G] {
    implicit def T = T0

    implicit def F = self

    implicit def G = G0
  }

  /**The product of Zips `F` and `G`, `[x](F[x], G[x]])`, is a Zip */
  def product[G[_]](implicit G0: Zip[G]): Zip[({type λ[α] = (F[α], G[α])})#λ] = new ProductZip[F, G] {
    implicit def F = self

    implicit def G = G0
  }

  def zipWith[A, B, C](fa: => F[A], fb: => F[B])(f: (A, B) => C)(implicit F: Functor[F]): F[C] =
    F.map(zip(fa, fb)) {
      case (a, b) => f(a, b)
    }

  def apzip[A, B](f: => F[A] => F[B], a: => F[A]): F[(A, B)] =
    zip(a, f(a))

  def apzipPL[A, B](f: => F[A] @?> F[B], a: => F[A])(implicit M: Monoid[F[B]]): F[(A, B)] =
    apzip(f.getOrZ(_), a)

  def ap(implicit F: Functor[F]): Apply[F] =
    new Apply[F] {
      def ap[A, B](fa: => F[A])(f: => F[A => B]) =
        zipWith(fa, f)((a, g) => g(a))
      def map[A, B](fa: F[A])(f: A => B) =
        F.map(fa)(f)
    }

  ////
  val zipSyntax = new scalaz.syntax.ZipSyntax[F] { def F = Zip.this }
}

object Zip {
  @inline def apply[F[_]](implicit F: Zip[F]): Zip[F] = F

  ////

  def fzip[F[_], A, B](t: LazyTuple2[F[A], F[B]])(implicit F: Zip[F]): F[(A, B)] =
      F.zip(t._1, t._2)
  ////
}

