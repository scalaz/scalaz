package scalaz

////
/**
 * Universally quantified [[scalaz.Semigroup]].
 */
////
trait Plus[F[_]]  { self =>
  ////

  /**The composition of Plus `F` and `G`, `[x]F[G[x]]`, is a Plus */
  def compose[G[_]]: Plus[λ[α => F[G[α]]]] =
    new CompositionPlus[F, G] {
      override def F = self
    }

  /**The product of Plus `F` and `G`, `[x](F[x], G[x]])`, is a Plus */
  def product[G[_]](implicit G0: Plus[G]): Plus[λ[α => (F[α], G[α])]] =
    new ProductPlus[F, G] {
      override def F = self
      override def G = G0
    }

  def plus[A](a: F[A], b: => F[A]): F[A]

  def semigroup[A]: Semigroup[F[A]] = new Semigroup[F[A]] {
    def append(f1: F[A], f2: => F[A]): F[A] = plus(f1, f2)
  }

  trait PlusLaw {
    def associative[A](f1: F[A], f2: F[A], f3: F[A])(implicit FA: Equal[F[A]]): Boolean =
      FA.equal(plus(f1, plus(f2, f3)), plus(plus(f1, f2), f3))
  }
  def plusLaw =
    new PlusLaw {}
  ////
  val plusSyntax: scalaz.syntax.PlusSyntax[F] =
    new scalaz.syntax.PlusSyntax[F] { def F = Plus.this }
}

object Plus {
  @inline def apply[F[_]](implicit F: Plus[F]): Plus[F] = F

  import Isomorphism._

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: Plus[G]): Plus[F] =
    new IsomorphismPlus[F, G] {
      override def G: Plus[G] = E
      override def iso: F <~> G = D
    }

  ////

  private[this] trait LiftedPlus[G[_], F[_]] extends Plus[λ[a => G[F[a]]]] {
    implicit def G: Apply[G]
    implicit def F: Plus[F]

    def plus[A](x: G[F[A]], y: => G[F[A]]): G[F[A]] = G.apply2(x, y)(F.plus(_, _))
  }

  def liftPlus[G[_], F[_]](implicit G0: Apply[G], F0: Plus[F]): Plus[λ[a => G[F[a]]]] =
    new LiftedPlus[G, F] {
      def G = G0
      def F = F0
    }

  ////
}
