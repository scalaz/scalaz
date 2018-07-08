package scalaz

////
// Copyright: 2017 Sam Halliday
// License: https://opensource.org/licenses/BSD-3-Clause

/**
 * Invariant parent of Decidable and Alternative.
 *
 * Used for typeclass derivation of products, coproducts and value types.
 */
////
trait InvariantAlt[F[_]] extends InvariantApplicative[F] { self =>
  ////

  def xcoproduct1[Z, A1](a1: =>F[A1])(f: A1 => Z, g: Z => A1): F[Z] =
    xmap(a1, f, g)
  def xcoproduct2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(
    f: A1 \/ A2 => Z,
    g: Z => A1 \/ A2
  ): F[Z]
  def xcoproduct3[Z, A1, A2, A3](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3])(
    f: A1 \/ (A2 \/ A3) => Z,
    g: Z => A1 \/ (A2 \/ A3)
  ): F[Z] = {
    val a23: F[A2 \/ A3] = xcoproduct2(a2, a3)(identity, identity)
    xcoproduct2(a1, a23)(f, g)
  }
  def xcoproduct4[Z, A1, A2, A3, A4](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4])(
    f: A1 \/ (A2 \/ (A3 \/ A4)) => Z,
    g: Z => A1 \/ (A2 \/ (A3 \/ A4))
  ): F[Z] = {
    val a234: F[A2 \/ (A3 \/ A4)] = xcoproduct3(a2, a3, a4)(identity, identity)
    xcoproduct2(a1, a234)(f, g)
  }

  // these methods fail for recursive ADTs, fixed in
  // https://github.com/scala/scala/pull/6050
  final def xcoderiving1[Z, A1](
    f: A1 => Z,
    g: Z => A1
  )(implicit a1: F[A1]): F[Z] = xcoproduct1(a1)(f, g)
  final def xcoderiving2[Z, A1, A2](
    f: (A1 \/ A2) => Z,
    g: Z => (A1 \/ A2)
  )(implicit a1: F[A1], a2: F[A2]): F[Z] = xcoproduct2(a1, a2)(f, g)
  final def xcoderiving3[Z, A1, A2, A3](
    f: (A1 \/ (A2 \/ A3)) => Z,
    g: Z => (A1 \/ (A2 \/ A3))
  )(implicit a1: F[A1], a2: F[A2], a3: F[A3]): F[Z] = xcoproduct3(a1, a2, a3)(f, g)
  final def xcoderiving4[Z, A1, A2, A3, A4](
    f: (A1 \/ (A2 \/ (A3 \/ A4))) => Z,
    g: Z => (A1 \/ (A2 \/ (A3 \/ A4)))
  )(implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4]): F[Z] = xcoproduct4(a1, a2, a3, a4)(f, g)

  ////
  val invariantAltSyntax = new scalaz.syntax.InvariantAltSyntax[F] { def F = InvariantAlt.this }
}

object InvariantAlt {
  @inline def apply[F[_]](implicit F: InvariantAlt[F]): InvariantAlt[F] = F

  import Isomorphism._

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: InvariantAlt[G]): InvariantAlt[F] =
    new IsomorphismInvariantAlt[F, G] {
      override def G: InvariantAlt[G] = E
      override def iso: F <~> G = D
    }

  ////

  ////
}

trait IsomorphismInvariantAlt[F[_], G[_]] extends InvariantAlt[F] with IsomorphismInvariantApplicative[F, G]{
  implicit def G: InvariantAlt[G]
  ////
  import Isomorphism._

  def iso: F <~> G

  def xcoproduct2[Z, A1, A2](a1: => F[A1], a2: => F[A2])(f: A1 \/ A2 => Z, g: Z => A1 \/ A2): F[Z] =
    iso.from(G.xcoproduct2(iso.to(a1), iso.to(a2))(f, g))

  ////
}
