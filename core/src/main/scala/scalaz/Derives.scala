// Copyright: 2017 Sam Halliday
// License: https://opensource.org/licenses/BSD-3-Clause

package scalaz

import scala.{ inline }

import shapeless.{ Cached, Lazy }

/** Invariant derivation of products and coproducts of limited arity. */
trait Derives[F[_]]
    extends CoapplicativeCodivide[F]
    with ApplicativeDivisible[F]
object Derives {
  @inline def apply[F[_]](
    implicit i: Derives[F]
  ): Derives[F] = i

  implicit val Equal: Derives[Equal] = new ContravariantDerives[Equal] {
    override def divide2[A1, A2, Z](a1: =>Equal[A1], a2: =>Equal[A2])(
      f: Z => (A1, A2)
    ): Equal[Z] = { (z1, z2) =>
      val (s1, s2) = f(z1)
      val (t1, t2) = f(z2)
      a1.equal(s1, t1) && a2.equal(s2, t2)
    }
    override def conquer[A]: Equal[A] = ((_, _) => true)

    override def codivide1[Z, A1](a1: =>Equal[A1])(f: Z => A1): Equal[Z] =
      ((z1, z2) => a1.equal(f(z1), f(z2)))

    override def codivide2[Z, A1, A2](a1: =>Equal[A1], a2: =>Equal[A2])(
      f: Z => A1 \/ A2
    ): Equal[Z] = { (z1, z2) =>
      (f(z1), f(z2)) match {
        case (-\/(s), -\/(t)) => a1.equal(s, t)
        case (\/-(s), \/-(t)) => a2.equal(s, t)
        case _                => false
      }
    }
  }
}

trait ContravariantDerives[F[_]]
    extends Derives[F]
    with Codivide[F]
    with LazyDivisible[F]
object ContravariantDerives {
  @inline def apply[F[_]](
    implicit i: ContravariantDerives[F]
  ): ContravariantDerives[F] = i
}

trait CovariantDerives[F[_]]
    extends Derives[F]
    with Coapplicative[F]
    with LazyApplicative[F]
//with DangerousApplicative[F]
object CovariantDerives {
  @inline def apply[F[_]](
    implicit i: CovariantDerives[F]
  ): CovariantDerives[F] = i
}

trait ApplyDivide[F[_]] extends InvariantFunctor[F] {

  def xproduct1[Z, A1](a1: F[A1])(f: A1 => Z, g: Z => A1): F[Z] = xmap(a1, f, g)
  def xproduct2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(f: (A1, A2) => Z,
                                                     g: Z => (A1, A2)): F[Z]
  def xproduct3[Z, A1, A2, A3](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3])(
    f: (A1, A2, A3) => Z,
    g: Z => (A1, A2, A3)
  ): F[Z]
  def xproduct4[Z, A1, A2, A3, A4](
    a1: =>F[A1],
    a2: =>F[A2],
    a3: =>F[A3],
    a4: =>F[A4]
  )(f: (A1, A2, A3, A4) => Z, g: Z => (A1, A2, A3, A4)): F[Z]

  final def xderiving1[Z, A1](f: A1 => Z, g: Z => A1)(
    implicit a1: Cached[Lazy[F[A1]]]
  ): F[Z] = xproduct1(a1.value.value)(f, g)
  final def xderiving2[Z, A1, A2](f: (A1, A2) => Z, g: Z => (A1, A2))(
    implicit a1: Cached[Lazy[F[A1]]],
    a2: Cached[Lazy[F[A2]]]
  ): F[Z] = xproduct2(a1.value.value, a2.value.value)(f, g)
  final def xderiving3[Z, A1, A2, A3](f: (A1, A2, A3) => Z,
                                      g: Z => (A1, A2, A3))(
    implicit
    a1: Cached[Lazy[F[A1]]],
    a2: Cached[Lazy[F[A2]]],
    a3: Cached[Lazy[F[A3]]]
  ): F[Z] = xproduct3(a1.value.value, a2.value.value, a3.value.value)(f, g)
  final def xderiving4[Z, A1, A2, A3, A4](f: (A1, A2, A3, A4) => Z,
                                          g: Z => (A1, A2, A3, A4))(
    implicit
    a1: Cached[Lazy[F[A1]]],
    a2: Cached[Lazy[F[A2]]],
    a3: Cached[Lazy[F[A3]]],
    a4: Cached[Lazy[F[A4]]]
  ): F[Z] =
    xproduct4(a1.value.value, a2.value.value, a3.value.value, a4.value.value)(f,
                                                                              g)
}
object ApplyDivide {
  @inline def apply[F[_]](implicit i: ApplyDivide[F]): ApplyDivide[F] = i
}

// an invariant parent of Applicative / Divisible
trait ApplicativeDivisible[F[_]] extends ApplyDivide[F] {
  def xproduct0[Z](f: =>Z): F[Z]
  final def xderiving0[Z](z: Z): F[Z] = xproduct0(z)
}
object ApplicativeDivisible {
  @inline def apply[F[_]](
    implicit i: ApplicativeDivisible[F]
  ): ApplicativeDivisible[F] = i
}

/** Invariant parent of Coapplicative and Codivide */
trait CoapplicativeCodivide[F[_]] {
  def xcoproduct1[Z, A1](a1: =>F[A1])(f: A1 => Z, g: Z => A1): F[Z]
  def xcoproduct2[Z, A1, A2](a1: =>F[A1], a2: =>F[A2])(f: A1 \/ A2 => Z,
                                                       g: Z => A1 \/ A2): F[Z]
  def xcoproduct3[Z, A1, A2, A3](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3])(
    f: A1 \/ (A2 \/ A3) => Z,
    g: Z => A1 \/ (A2 \/ A3)
  ): F[Z]
  def xcoproduct4[Z, A1, A2, A3, A4](
    a1: =>F[A1],
    a2: =>F[A2],
    a3: =>F[A3],
    a4: =>F[A4]
  )(f: A1 \/ (A2 \/ (A3 \/ A4)) => Z, g: Z => A1 \/ (A2 \/ (A3 \/ A4))): F[Z]

  def xcoderiving1[Z, A1](f: A1 => Z, g: Z => A1)(
    implicit a1: Cached[Lazy[F[A1]]]
  ): F[Z] = xcoproduct1(a1.value.value)(f, g)
  def xcoderiving2[Z, A1, A2](f: (A1 \/ A2) => Z, g: Z => (A1 \/ A2))(
    implicit a1: Cached[Lazy[F[A1]]],
    a2: Cached[Lazy[F[A2]]]
  ): F[Z] = xcoproduct2(a1.value.value, a2.value.value)(f, g)
  def xcoderiving3[Z, A1, A2, A3](f: (A1 \/ (A2 \/ A3)) => Z,
                                  g: Z => (A1 \/ (A2 \/ A3)))(
    implicit
    a1: Cached[Lazy[F[A1]]],
    a2: Cached[Lazy[F[A2]]],
    a3: Cached[Lazy[F[A3]]]
  ): F[Z] = xcoproduct3(a1.value.value, a2.value.value, a3.value.value)(f, g)
  def xcoderiving4[Z, A1, A2, A3, A4](f: (A1 \/ (A2 \/ (A3 \/ A4))) => Z,
                                      g: Z => (A1 \/ (A2 \/ (A3 \/ A4))))(
    implicit
    a1: Cached[Lazy[F[A1]]],
    a2: Cached[Lazy[F[A2]]],
    a3: Cached[Lazy[F[A3]]],
    a4: Cached[Lazy[F[A4]]]
  ): F[Z] =
    xcoproduct4(a1.value.value, a2.value.value, a3.value.value, a4.value.value)(
      f,
      g
    )
}
object CoapplicativeCodivide {
  @inline def apply[F[_]](
    implicit i: CoapplicativeCodivide[F]
  ): CoapplicativeCodivide[F] = i
}
