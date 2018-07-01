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
trait Derives[F[_]]  { self =>
  ////

  def xproduct0[Z](f: =>Z): F[Z]
  def xproduct1[Z, A1](a1: =>F[A1])(f: A1 => Z, g: Z => A1): F[Z]
  def xproduct2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(
    f: (A1, A2) => Z,
    g: Z => (A1, A2)
  ): F[Z]
  def xproduct3[Z, A1, A2, A3](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3])(
    f: (A1, A2, A3) => Z,
    g: Z => (A1, A2, A3)
  ): F[Z]
  def xproduct4[Z, A1, A2, A3, A4](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4])(
    f: (A1, A2, A3, A4) => Z,
    g: Z => (A1, A2, A3, A4)
  ): F[Z]

  def xcoproduct1[Z, A1](a1: =>F[A1])(f: A1 => Z, g: Z => A1): F[Z]
  def xcoproduct2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(
    f: A1 \/ A2 => Z,
    g: Z => A1 \/ A2
  ): F[Z]
  def xcoproduct3[Z, A1, A2, A3](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3])(
    f: A1 \/ (A2 \/ A3) => Z,
    g: Z => A1 \/ (A2 \/ A3)
  ): F[Z]
  def xcoproduct4[Z, A1, A2, A3, A4](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4])(
    f: A1 \/ (A2 \/ (A3 \/ A4)) => Z,
    g: Z => A1 \/ (A2 \/ (A3 \/ A4))
  ): F[Z]

  // these methods fail for recursive ADTs, fixed in
  // https://github.com/scala/scala/pull/6050
  final def xderiving0[Z](z: =>Z): F[Z] = xproduct0(z)
  final def xderiving1[Z, A1](
    f: A1 => Z,
    g: Z => A1
  )(implicit a1: F[A1]): F[Z] = xproduct1(a1)(f, g)
  final def xderiving2[Z, A1, A2](
    f: (A1, A2) => Z,
    g: Z => (A1, A2)
  )(implicit a1: F[A1], a2: F[A2]): F[Z] = xproduct2(a1, a2)(f, g)
  final def xderiving3[Z, A1, A2, A3](
    f: (A1, A2, A3) => Z,
    g: Z => (A1, A2, A3)
  )(implicit a1: F[A1], a2: F[A2], a3: F[A3]): F[Z] = xproduct3(a1, a2, a3)(f, g)
  final def xderiving4[Z, A1, A2, A3, A4](
    f: (A1, A2, A3, A4) => Z,
    g: Z => (A1, A2, A3, A4)
  )(implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4]): F[Z] = xproduct4(a1, a2, a3, a4)(f, g)
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
  val derivesSyntax = new scalaz.syntax.DerivesSyntax[F] { def F = Derives.this }
}

object Derives {
  @inline def apply[F[_]](implicit F: Derives[F]): Derives[F] = F

  import Isomorphism._

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: Derives[G]): Derives[F] =
    new IsomorphismDerives[F, G] {
      override def G: Derives[G] = E
      override def iso: F <~> G = D
    }

  ////

  ////
}

trait IsomorphismDerives[F[_], G[_]] extends Derives[F] {
  implicit def G: Derives[G]
  ////
  import Isomorphism._

  def iso: F <~> G

  def xcoproduct1[Z, A1](a1: => F[A1])(f: A1 => Z, g: Z => A1): F[Z] =
    iso.from(G.xcoproduct1(iso.to(a1))(f, g))
  def xcoproduct2[Z, A1, A2](a1: => F[A1], a2: => F[A2])(f: A1 \/ A2 => Z, g: Z => A1 \/ A2): F[Z] =
    iso.from(G.xcoproduct2(iso.to(a1), iso.to(a2))(f, g))
  def xcoproduct3[Z, A1, A2, A3](a1: => F[A1], a2: => F[A2], a3: => F[A3])(f: A1 \/ (A2 \/ A3) => Z, g: Z => A1 \/ (A2 \/ A3)): F[Z] =
    iso.from(G.xcoproduct3(iso.to(a1), iso.to(a2), iso.to(a3))(f, g))
  def xcoproduct4[Z, A1, A2, A3, A4](a1: => F[A1], a2: => F[A2], a3: => F[A3], a4: => F[A4])(f: A1 \/ (A2 \/ (A3 \/ A4)) => Z, g: Z => A1 \/ (A2 \/ (A3 \/ A4))): F[Z] =
    iso.from(G.xcoproduct4(iso.to(a1), iso.to(a2), iso.to(a3), iso.to(a4))(f, g))

  def xproduct0[Z](f: => Z): F[Z] =
    iso.from(G.xproduct0(f))
  def xproduct1[Z, A1](a1: => F[A1])(f: A1 => Z, g: Z => A1): F[Z] =
    iso.from(G.xproduct1(iso.to(a1))(f, g))
  def xproduct2[Z, A1, A2](a1: => F[A1], a2: => F[A2])(f: (A1, A2) => Z, g: Z => (A1, A2)): F[Z] =
    iso.from(G.xproduct2(iso.to(a1), iso.to(a2))(f, g))
  def xproduct3[Z, A1, A2, A3](a1: => F[A1], a2: => F[A2], a3: => F[A3])(f: (A1, A2, A3) => Z, g: Z => (A1, A2, A3)): F[Z] =
    iso.from(G.xproduct3(iso.to(a1), iso.to(a2), iso.to(a3))(f, g))
  def xproduct4[Z, A1, A2, A3, A4](a1: => F[A1], a2: => F[A2], a3: => F[A3], a4: => F[A4])(f: (A1, A2, A3, A4) => Z, g: Z => (A1, A2, A3, A4)): F[Z] =
    iso.from(G.xproduct4(iso.to(a1), iso.to(a2), iso.to(a3), iso.to(a4))(f, g))

  ////
}
