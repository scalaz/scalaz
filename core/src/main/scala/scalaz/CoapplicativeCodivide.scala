package scalaz

////
// Copyright: 2017 Sam Halliday
// License: https://opensource.org/licenses/BSD-3-Clause

/** Invariant parent of Coapplicative and Codivide
 *
 */
////
trait CoapplicativeCodivide[F[_]]  { self =>
  ////

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
  def xcoderiving1[Z, A1](
    f: A1 => Z,
    g: Z => A1
  )(implicit a1: F[A1]): F[Z] = xcoproduct1(a1)(f, g)
  def xcoderiving2[Z, A1, A2](
    f: (A1 \/ A2) => Z,
    g: Z => (A1 \/ A2)
  )(implicit a1: F[A1], a2: F[A2]): F[Z] = xcoproduct2(a1, a2)(f, g)
  def xcoderiving3[Z, A1, A2, A3](
    f: (A1 \/ (A2 \/ A3)) => Z,
    g: Z => (A1 \/ (A2 \/ A3))
  )(implicit a1: F[A1], a2: F[A2], a3: F[A3]): F[Z] = xcoproduct3(a1, a2, a3)(f, g)
  def xcoderiving4[Z, A1, A2, A3, A4](
    f: (A1 \/ (A2 \/ (A3 \/ A4))) => Z,
    g: Z => (A1 \/ (A2 \/ (A3 \/ A4)))
  )(implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4]): F[Z] = xcoproduct4(a1, a2, a3, a4)(f, g)

  ////
  val coapplicativeCodivideSyntax = new scalaz.syntax.CoapplicativeCodivideSyntax[F] { def F = CoapplicativeCodivide.this }
}

object CoapplicativeCodivide {
  @inline def apply[F[_]](implicit F: CoapplicativeCodivide[F]): CoapplicativeCodivide[F] = F

  import Isomorphism._

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: CoapplicativeCodivide[G]): CoapplicativeCodivide[F] =
    new IsomorphismCoapplicativeCodivide[F, G] {
      override def G: CoapplicativeCodivide[G] = E
      override def iso: F <~> G = D
    }

  ////

  ////
}

trait IsomorphismCoapplicativeCodivide[F[_], G[_]] extends CoapplicativeCodivide[F] {
  implicit def G: CoapplicativeCodivide[G]
  ////
  import Isomorphism._
  def iso: F <~> G

  override def xcoproduct1[Z, A1](a1: => F[A1])(f: A1 => Z, g: Z => A1): F[Z] =
    iso.from(G.xcoproduct1(iso.to(a1))(f, g))
  override def xcoproduct2[Z, A1, A2](a1: => F[A1], a2: => F[A2])(f: A1 \/ A2 => Z, g: Z => A1 \/ A2): F[Z] =
    iso.from(G.xcoproduct2(iso.to(a1), iso.to(a2))(f, g))
  override def xcoproduct3[Z, A1, A2, A3](a1: => F[A1], a2: => F[A2], a3: => F[A3])(f: A1 \/ (A2 \/ A3) => Z, g: Z => A1 \/ (A2 \/ A3)): F[Z] =
    iso.from(G.xcoproduct3(iso.to(a1), iso.to(a2), iso.to(a3))(f, g))
  override def xcoproduct4[Z, A1, A2, A3, A4](a1: => F[A1], a2: => F[A2], a3: => F[A3], a4: => F[A4])(f: A1 \/ (A2 \/ (A3 \/ A4)) => Z, g: Z => A1 \/ (A2 \/ (A3 \/ A4))): F[Z] =
    iso.from(G.xcoproduct4(iso.to(a1), iso.to(a2), iso.to(a3), iso.to(a4))(f, g))

  ////
}
