package scalaz

////
// Copyright: 2017 Sam Halliday
// License: https://opensource.org/licenses/BSD-3-Clause

/** Coproduct analogue of Divide
 *
 */
////
trait Codivide[F[_]] extends CoapplicativeCodivide[F] { self =>
  ////

  def codivide1[Z, A1](a1: =>F[A1])(f: Z => A1): F[Z]
  def codivide2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(f: Z => A1 \/ A2): F[Z]
  def codivide3[Z, A1, A2, A3](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3])(
    f: Z => A1 \/ (A2 \/ A3)
  ): F[Z] = {
    val a23: F[A2 \/ A3] = codivide2(a2, a3)(identity)
    codivide2(a1, a23)(f)
  }
  def codivide4[Z, A1, A2, A3, A4](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4])(
    f: Z => A1 \/ (A2 \/ (A3 \/ A4))
  ): F[Z] = {
    val a34: F[A3 \/ A4]          = codivide2(a3, a4)(identity)
    val a234: F[A2 \/ (A3 \/ A4)] = codivide2(a2, a34)(identity)
    codivide2(a1, a234)(f)
  }
  // ... codivideN

  final def codividing2[Z, A1, A2](
    f: Z => A1 \/ A2
  )(implicit fa1: F[A1], fa2: F[A2]): F[Z] =
    codivide2(fa1, fa2)(f)
  final def codividing3[Z, A1, A2, A3](
    f: Z => A1 \/ (A2 \/ A3)
  )(implicit fa1: F[A1], fa2: F[A2], fa3: F[A3]): F[Z] =
    codivide3(fa1, fa2, fa3)(f)
  final def codividing4[Z, A1, A2, A3, A4](
    f: Z => A1 \/ (A2 \/ (A3 \/ A4))
  )(implicit fa1: F[A1], fa2: F[A2], fa3: F[A3], fa4: F[A4]): F[Z] =
    codivide4(fa1, fa2, fa3, fa4)(f)
  // ... codividingX

  override def xcoproduct1[Z, A1](a1: =>F[A1])(
    f: A1 => Z,
    g: Z => A1
  ): F[Z] = codivide1(a1)(g)
  override def xcoproduct2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(
    f: (A1 \/ A2) => Z,
    g: Z => (A1 \/ A2)
  ): F[Z] = codivide2(a1, a2)(g)
  override def xcoproduct3[Z, A1, A2, A3](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3])(
    f: (A1 \/ (A2 \/ A3)) => Z,
    g: Z => (A1 \/ (A2 \/ A3))
  ): F[Z] = codivide3(a1, a2, a3)(g)
  override def xcoproduct4[Z, A1, A2, A3, A4](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3], a4: =>F[A4])(
    f: (A1 \/ (A2 \/ (A3 \/ A4))) => Z,
    g: Z => (A1 \/ (A2 \/ (A3 \/ A4)))
  ): F[Z] = codivide4(a1, a2, a3, a4)(g)

  ////
  val codivideSyntax = new scalaz.syntax.CodivideSyntax[F] { def F = Codivide.this }
}

object Codivide {
  @inline def apply[F[_]](implicit F: Codivide[F]): Codivide[F] = F

  import Isomorphism._

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: Codivide[G]): Codivide[F] =
    new IsomorphismCodivide[F, G] {
      override def G: Codivide[G] = E
      override def iso: F <~> G = D
    }

  ////

  ////
}

trait IsomorphismCodivide[F[_], G[_]] extends Codivide[F] with IsomorphismCoapplicativeCodivide[F, G]{
  implicit def G: Codivide[G]
  ////

  def codivide1[Z, A1](a1: => F[A1])(f: Z => A1): F[Z] =
    iso.from(G.codivide1(iso.to(a1))(f))

  def codivide2[Z, A1, A2](a1: => F[A1], a2: => F[A2])(f: Z => A1 \/ A2): F[Z] =
    iso.from(G.codivide2(iso.to(a1), iso.to(a2))(f))

  ////
}
