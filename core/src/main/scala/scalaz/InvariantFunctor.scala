package scalaz

////
/**
 * Unary type constructor that supports an `xmap` operation that converts an `F[A]` to an `F[B]` given
 * two functions, `A => B` and `B => A`.
 *
 * An invariant functor must satisfy two laws:
 *  - identity - xmap(ma)(identity, identity) == ma
 *  - composite - xmap(xmap(ma, f1, g1), f2, g2) == xmap(ma, f2 compose f1, g1, compose g2)
 *
 * Also known as an exponential functor.
 *
 * @see [[https://hackage.haskell.org/packages/archive/invariant/latest/doc/html/Data-Functor-Invariant.html]]
 * @see [[http://comonad.com/reader/2008/rotten-bananas/]]
 *
 * @see [[scalaz.InvariantFunctor.InvariantFunctorLaw]]
 */
////
trait InvariantFunctor[F[_]]  { self =>
  ////

  import BijectionT.Bijection
  import Isomorphism.<=>

  /** Converts `ma` to a value of type `F[B]` using the provided functions `f` and `g`. */
  def xmap[A, B](ma: F[A], f: A => B, g: B => A): F[B]

  /** Converts `ma` to a value of type `F[B]` using the provided bijection. */
  def xmapb[A, B](ma: F[A])(b: Bijection[A, B]): F[B] = xmap(ma, b.to, b.from)

  /** Converts `ma` to a value of type `F[B]` using the provided isomorphism. */
  def xmapi[A, B](ma: F[A])(iso: A <=> B): F[B] = xmap(ma, iso.to, iso.from)

  trait InvariantFunctorLaw {

    def invariantIdentity[A](fa: F[A])(implicit FA: Equal[F[A]]): Boolean =
      FA.equal(xmap[A, A](fa, x => x, x => x), fa)

    def invariantComposite[A, B, C](fa: F[A], f1: A => B, g1: B => A, f2: B => C, g2: C => B)(implicit FC: Equal[F[C]]): Boolean =
      FC.equal(xmap(xmap(fa, f1, g1), f2, g2), xmap(fa, f2 compose f1, g1 compose g2))
  }

  def invariantFunctorLaw = new InvariantFunctorLaw {}
  ////
  val invariantFunctorSyntax = new scalaz.syntax.InvariantFunctorSyntax[F] { def F = InvariantFunctor.this }
}

object InvariantFunctor {
  @inline def apply[F[_]](implicit F: InvariantFunctor[F]): InvariantFunctor[F] = F

  import Isomorphism._

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: InvariantFunctor[G]): InvariantFunctor[F] =
    new IsomorphismInvariantFunctor[F, G] {
      override def G: InvariantFunctor[G] = E
      override def iso: F <~> G = D
    }

  ////
  ////
}

trait IsomorphismInvariantFunctor[F[_], G[_]] extends InvariantFunctor[F] {
  implicit def G: InvariantFunctor[G]
  ////
  import Isomorphism._

  def iso: F <~> G

  override def xmap[A, B](ma: F[A], f: A => B, g: B => A): F[B] =
    iso.from(G.xmap(iso.to(ma), f, g))
  ////
}
