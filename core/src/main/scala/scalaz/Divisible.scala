package scalaz

////
/** `Divisible` is the contravariant analogue of `scalaz.Applicative`
 *
 * @see [[https://github.com/ekmett/contravariant/blob/v1.3.2/src/Data/Functor/Contravariant/Divisible.hs]]
 * @see [[https://youtu.be/cB8DapKQz-I?t=20m35s ZuriHac 2015 - Discrimination is Wrong: Improving Productivity]]
 */
////
trait Divisible[F[_]] extends Divide[F] with InvariantApplicative[F] { self =>
  ////
  /** Universally quantified instance of F[_] */
  def conquer[A]: F[A]

  override def contramap[A, B](fa: F[A])(f: B => A): F[B] =
    divide2(conquer[Unit], fa)(c => ((), f(c)))

  override def xproduct0[Z](z: =>Z): F[Z] = conquer
  override def xproduct1[Z, A1](a1: =>F[A1])(f: A1 => Z, g: Z => A1): F[Z] = xmap(a1, f, g)
  override def xproduct2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(
    f: (A1, A2) => Z,
    g: Z => (A1, A2)
  ): F[Z] = divide2(a1, a2)(g)
  override def xproduct3[Z, A1, A2, A3](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3])(
    f: (A1, A2, A3) => Z,
    g: Z => (A1, A2, A3)
  ): F[Z] = divide3(a1, a2, a3)(g)
  override def xproduct4[Z, A1, A2, A3, A4](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4])(
    f: (A1, A2, A3, A4) => Z,
    g: Z => (A1, A2, A3, A4)
  ): F[Z] = divide4(a1, a2, a3, a4)(g)

  trait DivisibleLaw extends DivideLaw {
    def rightIdentity[A](fa: F[A])(implicit E: Equal[F[A]]): Boolean =
      E.equal(divide(fa, conquer[A])(delta), fa)

    def leftIdentity[A](fa: F[A])(implicit E: Equal[F[A]]): Boolean =
      E.equal(divide(conquer[A], fa)(delta), fa)
  }

  def divisibleLaw: DivisibleLaw = new DivisibleLaw {}

  ////
  val divisibleSyntax: scalaz.syntax.DivisibleSyntax[F] =
    new scalaz.syntax.DivisibleSyntax[F] { def F = Divisible.this }
}

object Divisible {
  @inline def apply[F[_]](implicit F: Divisible[F]): Divisible[F] = F

  import Isomorphism._

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: Divisible[G]): Divisible[F] =
    new IsomorphismDivisible[F, G] {
      override def G: Divisible[G] = E
      override def iso: F <~> G = D
    }

  ////

  ////
}

trait IsomorphismDivisible[F[_], G[_]] extends Divisible[F] with IsomorphismDivide[F, G] with IsomorphismInvariantApplicative[F, G]{
  implicit def G: Divisible[G]
  ////

  override def conquer[A]: F[A] =
    iso.from(G.conquer)

  override def xproduct0[Z](z: => Z): F[Z] =
    super[Divisible].xproduct0(z)
  override def xproduct1[Z, A1](a1: =>F[A1])(f: A1 => Z, g: Z => A1): F[Z] =
    super[Divisible].xproduct1(a1)(f, g)
  override def xproduct2[Z, A1, A2](a1: => F[A1], a2: => F[A2])(f: (A1, A2) => Z, g: Z => (A1, A2)): F[Z] =
    super[Divisible].xproduct2(a1, a2)(f, g)
  override def xproduct3[Z, A1, A2, A3](a1: => F[A1], a2: => F[A2], a3: => F[A3])(f: (A1, A2, A3) => Z, g: Z => (A1, A2, A3)): F[Z] =
    super[Divisible].xproduct3(a1, a2, a3)(f, g)
  override def xproduct4[Z, A1, A2, A3, A4](a1: => F[A1], a2: => F[A2], a3: => F[A3], a4: => F[A4])(f: (A1, A2, A3, A4) => Z, g: Z => (A1, A2, A3, A4)): F[Z] =
    super[Divisible].xproduct4(a1, a2, a3, a4)(f, g)

  ////
}
