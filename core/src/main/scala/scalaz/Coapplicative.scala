package scalaz

////
// Copyright: 2017 Sam Halliday
// License: https://opensource.org/licenses/BSD-3-Clause

/**
 * Coproduct analogue of Applicative
 */
////
trait Coapplicative[F[_]] extends CoapplicativeCodivide[F] { self =>
  ////

  def coapply1[Z, A1](a1: =>F[A1])(f: A1 => Z): F[Z]
  def coapply2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(f: A1 \/ A2 => Z): F[Z]
  def coapply3[Z, A1, A2, A3](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3])(
    f: A1 \/ (A2 \/ A3) => Z
  ): F[Z] = coapply2(a1, either2(a2, a3))(f)
  def coapply4[Z, A1, A2, A3, A4](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3],a4: =>F[A4])(
    f: A1 \/ (A2 \/ (A3 \/ A4)) => Z
  ): F[Z] = coapply2(a1, either2(a2, either2(a3, a4)))(f)
  // ... coapplyN

  // equivalent of tupleN
  def either2[A1, A2](a1: =>F[A1], a2: =>F[A2]): F[A1 \/ A2] =
    coapply2(a1, a2)(identity)
  // ... eitherN

  final def coapplying1[Z, A1](
    f: A1 => Z
  )(implicit a1: F[A1]): F[Z] =
    coapply1(a1)(f)
  final def coapplying2[Z, A1, A2](
    f: A1 \/ A2 => Z
  )(implicit a1: F[A1], a2: F[A2]): F[Z] =
    coapply2(a1, a2)(f)
  final def coapplying3[Z, A1, A2, A3](
    f: A1 \/ (A2 \/ A3) => Z
  )(implicit a1: F[A1], a2: F[A2], a3: F[A3]): F[Z] =
    coapply3(a1, a2, a3)(f)
  final def coapplying4[Z, A1, A2, A3, A4](
    f: A1 \/ (A2 \/ (A3 \/ A4)) => Z
  )(implicit a1: F[A1], a2: F[A2], a3: F[A3], a4: F[A4]): F[Z] =
    coapply4(a1, a2, a3, a4)(f)
  // ... coapplyingX

  override def xcoproduct1[Z, A1](a1: =>F[A1])(
    f: A1 => Z,
    g: Z => A1
  ): F[Z] = coapply1(a1)(f)
  override def xcoproduct2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(
    f: (A1 \/ A2) => Z,
    g: Z => (A1 \/ A2)
  ): F[Z] = coapply2(a1, a2)(f)
  override def xcoproduct3[Z, A1, A2, A3](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3])(
    f: (A1 \/ (A2 \/ A3)) => Z,
    g: Z => (A1 \/ (A2 \/ A3))
  ): F[Z] = coapply3(a1, a2, a3)(f)
  override def xcoproduct4[Z, A1, A2, A3, A4](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4])(
    f: (A1 \/ (A2 \/ (A3 \/ A4))) => Z,
    g: Z => (A1 \/ (A2 \/ (A3 \/ A4)))
  ): F[Z] = coapply4(a1, a2, a3, a4)(f)

  ////
  val coapplicativeSyntax = new scalaz.syntax.CoapplicativeSyntax[F] { def F = Coapplicative.this }
}

object Coapplicative {
  @inline def apply[F[_]](implicit F: Coapplicative[F]): Coapplicative[F] = F

  import Isomorphism._

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: Coapplicative[G]): Coapplicative[F] =
    new IsomorphismCoapplicative[F, G] {
      override def G: Coapplicative[G] = E
      override def iso: F <~> G = D
    }

  ////

  ////
}

trait IsomorphismCoapplicative[F[_], G[_]] extends Coapplicative[F] with IsomorphismCoapplicativeCodivide[F, G]{
  implicit def G: Coapplicative[G]
  ////

  def coapply1[Z, A1](a1: => F[A1])(f: A1 => Z): F[Z] =
    iso.from(G.coapply1(iso.to(a1))(f))

  def coapply2[Z, A1, A2](a1: => F[A1], a2: => F[A2])(f: A1 \/ A2 => Z): F[Z] =
    iso.from(G.coapply2(iso.to(a1), iso.to(a2))(f))

  ////
}
