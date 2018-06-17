package scalaz

////
// Copyright: 2017 Sam Halliday
// License: https://opensource.org/licenses/BSD-3-Clause

/** Invariant parent of Coapplicative and Decidable
 *
 */
////
trait CoapplicativeDecidable[F[_]]  { self =>
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
  val coapplicativeDecidableSyntax = new scalaz.syntax.CoapplicativeDecidableSyntax[F] { def F = CoapplicativeDecidable.this }
}

object CoapplicativeDecidable {
  @inline def apply[F[_]](implicit F: CoapplicativeDecidable[F]): CoapplicativeDecidable[F] = F

  import Isomorphism._

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: CoapplicativeDecidable[G]): CoapplicativeDecidable[F] =
    new IsomorphismCoapplicativeDecidable[F, G] {
      override def G: CoapplicativeDecidable[G] = E
      override def iso: F <~> G = D
    }

  ////

  ////
}

trait IsomorphismCoapplicativeDecidable[F[_], G[_]] extends CoapplicativeDecidable[F] {
  implicit def G: CoapplicativeDecidable[G]
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
