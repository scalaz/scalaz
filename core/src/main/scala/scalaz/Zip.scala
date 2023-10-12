package scalaz

////
/**
 *
 */
////
trait Zip[F[_]]  { self =>
  ////
  def zip[A, B](a: => F[A], b: => F[B]): F[(A, B)]

  // derived functions

  /**The composition of Zip `F` and `G`, `[x]F[G[x]]`, is a Zip (if F is a Functor) */
  def compose[G[_]](implicit T0: Functor[F], G0: Zip[G]): Zip[λ[α => F[G[α]]]] =
    new CompositionZip[F, G] {
      override def T = T0
      override def F = self
      override def G = G0
    }

  /**The product of Zips `F` and `G`, `[x](F[x], G[x]])`, is a Zip */
  def product[G[_]](implicit G0: Zip[G]): Zip[λ[α => (F[α], G[α])]] =
    new ProductZip[F, G] {
      override def F = self
      override def G = G0
    }

  def zipWith[A, B, C](fa: => F[A], fb: => F[B])(f: (A, B) => C)(implicit F: Functor[F]): F[C] =
    F.map(zip(fa, fb)) {
      case (a, b) => f(a, b)
    }

  def apzip[A, B](f: => F[A] => F[B], a: => F[A]): F[(A, B)] =
    zip(a, f(a))

  def apzipPL[A, B](f: => F[A] @?> F[B], a: => F[A])(implicit M: Monoid[F[B]]): F[(A, B)] =
    apzip(f.getOrZ, a)

  def ap(implicit F: Functor[F]): Apply[F] =
    new Apply[F] {
      def ap[A, B](fa: => F[A])(f: => F[A => B]) =
        zipWith(fa, f)((a, g) => g(a))
      def map[A, B](fa: F[A])(f: A => B) =
        F.map(fa)(f)
      override def apply2[A, B, C](fa: => F[A], fb: => F[B])(f: (A, B) => C) =
        zipWith(fa, fb)(f)
      override def tuple2[A, B](fa: => F[A], fb: => F[B]) =
        self.zip(fa, fb)
    }

  trait ZipLaw {
    /** Zipping preserves structure. */
    def zipPreservation[A](fa: F[A])(implicit FA: Equal[F[A]], F: Functor[F]): Boolean = {
      val fab = zip(fa, fa)
      FA.equal(F.map(fab)(_._1), fa) && FA.equal(F.map(fab)(_._2), fa)
    }

    def zipSymmetric[A, B](fa: F[A], fb: F[B])(implicit FA: Equal[F[A]], F: Functor[F]): Boolean =
      FA.equal(F.map(zip(fa, fb))(_._1), F.map(zip(fb, fa))(_._2))
  }
  def zipLaw = new ZipLaw {}

  ////
  val zipSyntax: scalaz.syntax.ZipSyntax[F] =
    new scalaz.syntax.ZipSyntax[F] { def F = Zip.this }
}

object Zip {
  @inline def apply[F[_]](implicit F: Zip[F]): Zip[F] = F

  import Isomorphism._

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: Zip[G]): Zip[F] =
    new IsomorphismZip[F, G] {
      override def G: Zip[G] = E
      override def iso: F <~> G = D
    }

  ////

  def fzip[F[_], A, B](t: LazyTuple2[F[A], F[B]])(implicit F: Zip[F]): F[(A, B)] =
      F.zip(t._1, t._2)

  implicit def idInstance: Zip[Id.Id] = Id.id
  ////
}
