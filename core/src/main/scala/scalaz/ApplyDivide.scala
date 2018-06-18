package scalaz

////
// Copyright: 2017 Sam Halliday
// License: https://opensource.org/licenses/BSD-3-Clause

/**
 *
 */
////
trait ApplyDivide[F[_]] extends InvariantFunctor[F] { self =>
  ////

  def xproduct1[Z, A1](a1: F[A1])(f: A1 => Z, g: Z => A1): F[Z] = xmap(a1, f, g)
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

  // these methods fail for recursive ADTs, fixed in
  // https://github.com/scala/scala/pull/6050
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

  ////
  val applyDivideSyntax = new scalaz.syntax.ApplyDivideSyntax[F] { def F = ApplyDivide.this }
}

object ApplyDivide {
  @inline def apply[F[_]](implicit F: ApplyDivide[F]): ApplyDivide[F] = F

  import Isomorphism._

  def fromIso[F[_], G[_]](D: F <~> G)(implicit E: ApplyDivide[G]): ApplyDivide[F] =
    new IsomorphismApplyDivide[F, G] {
      override def G: ApplyDivide[G] = E
      override def iso: F <~> G = D
    }

  ////

  ////
}

trait IsomorphismApplyDivide[F[_], G[_]] extends ApplyDivide[F] with IsomorphismInvariantFunctor[F, G]{
  implicit def G: ApplyDivide[G]
  ////

  override def xproduct2[Z, A1, A2](a1: => F[A1], a2: => F[A2])(f: (A1, A2) => Z, g: Z => (A1, A2)): F[Z] =
    iso.from(
      G.xproduct2(iso.to(a1), iso.to(a2))(f, g)
    )

  override def xproduct3[Z, A1, A2, A3](a1: => F[A1], a2: => F[A2], a3: => F[A3])(f: (A1, A2, A3) => Z, g: Z => (A1, A2, A3)): F[Z] =
    iso.from(
      G.xproduct3(iso.to(a1), iso.to(a2), iso.to(a3))(f, g)
    )

  override def xproduct4[Z, A1, A2, A3, A4](a1: => F[A1], a2: => F[A2], a3: => F[A3], a4: => F[A4])(f: (A1, A2, A3, A4) => Z, g: Z => (A1, A2, A3, A4)): F[Z] =
    iso.from(
      G.xproduct4(iso.to(a1), iso.to(a2), iso.to(a3), iso.to(a4))(f, g)
    )

  ////
}
