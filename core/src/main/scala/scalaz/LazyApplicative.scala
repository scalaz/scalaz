// Copyright: 2017 Sam Halliday
// License: https://opensource.org/licenses/BSD-3-Clause

package scalaz

import scala.{ inline }

import shapeless.{ Cached, Lazy }

// a variant of Applicative introducing the new hierarchy. May be merged into
// Applicative in scalaz7.3
trait LazyApplicative[F[_]]
    extends Applicative[F]
    with ApplicativeDivisible[F] {
  final def applying1[Z, A1](f: A1 => Z)(
    implicit a1: Cached[Lazy[F[A1]]]
  ): F[Z] = map(a1.value.value)(f)
  final def applying2[Z, A1, A2](
    f: (A1, A2) => Z
  )(implicit a1: Cached[Lazy[F[A1]]], a2: Cached[Lazy[F[A2]]]): F[Z] =
    apply2(a1.value.value, a2.value.value)(f)
  final def applying3[Z, A1, A2, A3](
    f: (A1, A2, A3) => Z
  )(implicit a1: Cached[Lazy[F[A1]]],
    a2: Cached[Lazy[F[A2]]],
    a3: Cached[Lazy[F[A3]]]): F[Z] =
    apply3(a1.value.value, a2.value.value, a3.value.value)(f)
  final def applying4[Z, A1, A2, A3, A4](
    f: (A1, A2, A3, A4) => Z
  )(implicit a1: Cached[Lazy[F[A1]]],
    a2: Cached[Lazy[F[A2]]],
    a3: Cached[Lazy[F[A3]]],
    a4: Cached[Lazy[F[A4]]]): F[Z] =
    apply4(a1.value.value, a2.value.value, a3.value.value, a4.value.value)(f)
  // ... applyingX

  // ApplicativeDivisible impl
  final override def xproduct0[Z](z: =>Z): F[Z] = pure(z)
  override def xproduct2[Z, A1, A2](
    a1: =>F[A1],
    a2: =>F[A2]
  )(f: (A1, A2) => Z, g: Z => (A1, A2)): F[Z] =
    apply2(a1, a2)(f)
  override def xproduct3[Z, A1, A2, A3](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3])(
    f: (A1, A2, A3) => Z,
    g: Z => (A1, A2, A3)
  ): F[Z] = apply3(a1, a2, a3)(f)
  override def xproduct4[Z, A1, A2, A3, A4](
    a1: =>F[A1],
    a2: =>F[A2],
    a3: =>F[A3],
    a4: =>F[A4]
  )(f: (A1, A2, A3, A4) => Z, g: Z => (A1, A2, A3, A4)): F[Z] =
    apply4(a1, a2, a3, a4)(f)

}
object LazyApplicative {
  @inline def apply[F[_]](implicit i: LazyApplicative[F]): LazyApplicative[F] =
    i
}
