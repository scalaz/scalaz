package scalaz

////
import scala.annotation.tailrec
import scalaz.Maybe.Just

/**
 * Universally quantified [[scalaz.Semigroup]].
 */
////
trait Plus[F[_]]  { self =>
  ////

  def plus[A](a: F[A], b: => F[A]): F[A]

  /**
   * Unfold `seed` to the left and sum using [[#plus]].
   * `Plus` instances with right absorbing elements may override this method
   * to not unfold more than is necessary to determine the result.
   */
  def unfoldlPsumOpt[S, A](seed: S)(f: S => Maybe[(S, F[A])]): Maybe[F[A]] = {
    @tailrec def go(s: S, acc: F[A]): F[A] = f(s) match {
      case Just((s, fa)) => go(s, plus(fa, acc))
      case _ => acc
    }
    f(seed) map { case (s, a) => go(s, a) }
  }

  /**
   * Unfold `seed` to the right and sum using [[#plus]].
   * `Plus` instances with left absorbing elements may override this method
   * to not unfold more than is necessary to determine the result.
   */
  def unfoldrPsumOpt[S, A](seed: S)(f: S => Maybe[(F[A], S)]): Maybe[F[A]] = {
    @tailrec def go(acc: F[A], s: S): F[A] = f(s) match {
      case Just((fa, s)) => go(plus(acc, fa), s)
      case _ => acc
    }
    f(seed) map { case (a, s) => go(a, s) }
  }


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

  def semigroup[A]: Semigroup[F[A]] = new Semigroup[F[A]] {
    def append(f1: F[A], f2: => F[A]): F[A] = plus(f1, f2)

    override def unfoldlSumOpt[S](seed: S)(f: S => Maybe[(S, F[A])]): Maybe[F[A]] =
      unfoldlPsumOpt(seed)(f)

    override def unfoldrSumOpt[S](seed: S)(f: S => Maybe[(F[A], S)]): Maybe[F[A]] =
      unfoldrPsumOpt(seed)(f)
  }

  trait PlusLaw {
    def associative[A](f1: F[A], f2: F[A], f3: F[A])(implicit FA: Equal[F[A]]): Boolean =
      FA.equal(plus(f1, plus(f2, f3)), plus(plus(f1, f2), f3))
  }
  def plusLaw: PlusLaw =
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

  private[scalaz] trait LiftedPlus[G[_], F[_]] extends Plus[λ[a => G[F[a]]]] {
    implicit def G: Apply[G]
    implicit def F: Plus[F]

    def plus[A](x: G[F[A]], y: => G[F[A]]): G[F[A]] = G.apply2(x, y)(F.plus(_, _))

    override def unfoldrPsumOpt[S, A](seed: S)(f: S => Maybe[(G[F[A]], S)]): Maybe[G[F[A]]] =
      G.unfoldrOpt(seed)(f)(using Reducer.identityReducer[F[A]](using F.semigroup))
  }

  def liftPlus[G[_], F[_]](implicit G0: Apply[G], F0: Plus[F]): Plus[λ[a => G[F[a]]]] =
    new LiftedPlus[G, F] {
      def G = G0
      def F = F0
    }

  ////
}

trait IsomorphismPlus[F[_], G[_]] extends Plus[F] {
  implicit def G: Plus[G]
  ////
  import Isomorphism._

  def iso: F <~> G

  def plus[A](a: F[A], b: => F[A]): F[A] =
    iso.from(G.plus(iso.to(a), iso.to(b)))
  ////
}
