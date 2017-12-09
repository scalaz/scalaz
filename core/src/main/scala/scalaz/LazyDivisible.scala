// Copyright: 2017 Sam Halliday
// License: https://opensource.org/licenses/BSD-3-Clause

package scalaz

import scala.{ inline, Unit }
import scala.Predef.identity

import shapeless.{ Cached, Lazy }

// copy/pasta of scalaz.Divide with by-name params and "Lazy" (Name)
// types (will hopefully replace Divide in scalaz7.3)
trait LazyDivide[F[_]] extends Contravariant[F] with ApplyDivide[F] {

  final def divide1[A1, Z](a1: F[A1])(f: Z => A1): F[Z] = contramap(a1)(f)

  def divide2[A1, A2, Z](a1: =>F[A1], a2: =>F[A2])(f: Z => (A1, A2)): F[Z]
  def divide3[A1, A2, A3, Z](a1: =>F[A1], a2: =>F[A2], a3: =>F[A3])(
    f: Z => (A1, A2, A3)
  ): F[Z] =
    divide2(tuple2(a1, a2), a3) { z =>
      val t = f(z)
      ((t._1, t._2), t._3)
    }
  def divide4[A1, A2, A3, A4, Z](a1: =>F[A1],
                                 a2: =>F[A2],
                                 a3: =>F[A3],
                                 a4: =>F[A4])(
    f: Z => (A1, A2, A3, A4)
  ): F[Z] =
    divide2(tuple2(a1, a2), tuple2(a3, a4)) { z =>
      val t = f(z)
      ((t._1, t._2), (t._3, t._4))
    }

  def tuple2[A1, A2](a1: =>F[A1], a2: =>F[A2]): F[(A1, A2)] =
    divide2(a1, a2)(identity)

  final def dividing1[A1, Z](
    f: Z => A1
  )(implicit a1: Cached[Lazy[F[A1]]]): F[Z] =
    divide1(a1.value.value)(f)
  final def dividing2[A1, A2, Z](
    f: Z => (A1, A2)
  )(implicit a1: Cached[Lazy[F[A1]]], a2: Cached[Lazy[F[A2]]]): F[Z] =
    divide2(a1.value.value, a2.value.value)(f)
  final def dividing3[A1, A2, A3, Z](
    f: Z => (A1, A2, A3)
  )(implicit a1: Cached[Lazy[F[A1]]],
    a2: Cached[Lazy[F[A2]]],
    a3: Cached[Lazy[F[A3]]]): F[Z] =
    divide3(a1.value.value, a2.value.value, a3.value.value)(f)
  final def dividing4[A1, A2, A3, A4, Z](
    f: Z => (A1, A2, A3, A4)
  )(implicit a1: Cached[Lazy[F[A1]]],
    a2: Cached[Lazy[F[A2]]],
    a3: Cached[Lazy[F[A3]]],
    a4: Cached[Lazy[F[A4]]]): F[Z] =
    divide4(a1.value.value, a2.value.value, a3.value.value, a4.value.value)(f)

  // ApplyDivide impl
  override final def xproduct2[Z, A1, A2](
    a1: =>F[A1],
    a2: =>F[A2]
  )(f: (A1, A2) => Z, g: Z => (A1, A2)): F[Z] =
    divide2(a1, a2)(g)
  override final def xproduct3[Z, A1, A2, A3](a1: =>F[A1],
                                              a2: =>F[A2],
                                              a3: =>F[A3])(
    f: (A1, A2, A3) => Z,
    g: Z => (A1, A2, A3)
  ): F[Z] = divide3(a1, a2, a3)(g)
  override final def xproduct4[Z, A1, A2, A3, A4](
    a1: =>F[A1],
    a2: =>F[A2],
    a3: =>F[A3],
    a4: =>F[A4]
  )(f: (A1, A2, A3, A4) => Z, g: Z => (A1, A2, A3, A4)): F[Z] =
    divide4(a1, a2, a3, a4)(g)

}
object LazyDivide {
  @inline def apply[F[_]](implicit i: LazyDivide[F]): LazyDivide[F] = i
}

// will hopefully replace Divisible in scalaz7.3
trait LazyDivisible[F[_]] extends LazyDivide[F] with ApplicativeDivisible[F] {
  // universally quantified instance of F[_]
  def conquer[A]: F[A]

  override def contramap[A, B](fa: F[A])(f: B => A): F[B] =
    divide2(conquer[Unit], fa)(c => ((), f(c)))

  // ApplicativeDivisible impl
  override final def xproduct0[Z](z: =>Z): F[Z] = conquer
}
object LazyDivisible {
  @inline def apply[F[_]](implicit i: LazyDivisible[F]): LazyDivisible[F] = i

  import java.lang.String
  import scala.StringContext

  sealed trait L[F[_], A]
  final case class Label[F[_], A](fa: F[A], label: String) extends L[F, A]
  final case class Solo[F[_], A](fa: F[A])                 extends L[F, A]

  implicit val tcdShow: LazyDivisible[L[Show, ?]] =
    new LazyDivisible[L[Show, ?]] {
      def conquer[A]: L[Show, A] = Solo(Show.shows(_ => ""))
      def divide2[A, B, C](fa: =>L[Show, A], fb: =>L[Show, B])(
        f: C => (A, B)
      ): L[Show, C] = Solo(
        Show.shows { c =>
          val (a, b) = f(c)
          (fa, fb) match {
            case (Label(fa, la), Label(fb, lb)) =>
              s"$la=${fa.shows(a)},$lb=${fb.shows(b)}"
            case (Label(fa, la), Solo(fb)) =>
              s"$la=${fa.shows(a)},${fb.shows(b)}"
            case (Solo(fa), Label(fb, lb)) =>
              s"${fa.shows(a)},$lb=${fb.shows(b)}"
            case (Solo(fa), Solo(fb)) =>
              s"${fa.shows(a)},${fb.shows(b)}"
          }
        }
      )
    }
}
