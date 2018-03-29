package scalaz

////
/**
 * Universally quantified [[scalaz.Monoid]].
 */
////
trait PlusEmpty[F[_]] extends Plus[F] { self =>
  ////
  def empty[A]: F[A]

  def unfoldlPsum[S, A](seed: S)(f: S => Maybe[(S, F[A])]): F[A] =
    unfoldlPsumOpt(seed)(f) getOrElse empty

  def unfoldrPsum[S, A](seed: S)(f: S => Maybe[(F[A], S)]): F[A] =
    unfoldrPsumOpt(seed)(f) getOrElse empty

  /**The composition of PlusEmpty `F` and `G`, `[x]F[G[x]]`, is a PlusEmpty */
  override def compose[G[_]]: PlusEmpty[λ[α => F[G[α]]]] =
    new CompositionPlusEmpty[F, G] {
      implicit def F = self
    }

  /**The product of PlusEmpty `F` and `G`, `[x](F[x], G[x]])`, is a PlusEmpty */
  def product[G[_]](implicit G0: PlusEmpty[G]): PlusEmpty[λ[α => (F[α], G[α])]] =
    new ProductPlusEmpty[F, G] {
      implicit def F = self
      implicit def G = G0
    }

  // derived functions

  def monoid[A]: Monoid[F[A]] =
    new Monoid[F[A]] {
      def append(f1: F[A], f2: => F[A]): F[A] = plus(f1, f2)
      def zero: F[A] = empty[A]

      override def unfoldlSum[S](seed: S)(f: S => Maybe[(S, F[A])]): F[A] = unfoldlPsum(seed)(f)
      override def unfoldrSum[S](seed: S)(f: S => Maybe[(F[A], S)]): F[A] = unfoldrPsum(seed)(f)
    }

  trait EmptyLaw extends PlusLaw {
    def rightPlusIdentity[A](f1: F[A])(implicit FA: Equal[F[A]]): Boolean =
      FA.equal(plus(f1, empty[A]), f1)

    def leftPlusIdentity[A](f1: F[A])(implicit FA: Equal[F[A]]): Boolean =
      FA.equal(plus(empty[A], f1), f1)
  }

  def plusEmptyLaw =
    new EmptyLaw {}

  ////
  val plusEmptySyntax = new scalaz.syntax.PlusEmptySyntax[F] { def F = PlusEmpty.this }
}

object PlusEmpty {
  @inline def apply[F[_]](implicit F: PlusEmpty[F]): PlusEmpty[F] = F

  ////
  implicit def liftPlusEmpty[M[_], N[_]](implicit M: Applicative[M], P: PlusEmpty[N]): PlusEmpty[λ[α => M[N[α]]]] =
    new Plus.LiftedPlus[M, N] with PlusEmpty[λ[α => M[N[α]]]] {
      def G = M
      def F = P
      def empty[A] = M.point(P.empty[A])
    }
  ////
}
