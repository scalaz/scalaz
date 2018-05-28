package scalaz
package data

import scala.AnyVal

import Prelude._
import AList.aListOps
import scalaz.ct.ComposeClass

/**
 * Binary counter-like accumulator for type-aligned binary type constructors,
 * with the most significant bit on the right and addition of new elements (i.e. "increment") from the left.
 */
final class PreComposeBalancer[F[_, _], A, B] private (count: Int, stack: AList1[F, A, B]) {

  /** Pre-compose an element. */
  def +:[Z](f: F[Z, A])(implicit F: Compose[F]): PreComposeBalancer[F, Z, B] =
    add(f, stack, 1, count)

  def result(implicit F: Compose[F]): F[A, B] =
    stack.tail.foldLeft(stack.head)(RightAction.compose(F))

  private def add[X, Y](h: F[X, Y], t: AList1[F, Y, B], hcount: Int, tfactor: Int)(
    implicit F: Compose[F]
  ): PreComposeBalancer[F, X, B] =
    // hcount: number of elemnts composed in the head (`h`)
    // tfactor: how many times more elements are there in tail (`t`) than in head (tcount = hcount * tfactor)
    if (tfactor % 2 == 0) new PreComposeBalancer(hcount * (tfactor + 1), h :: t)
    else {
      val h1      = F.compose(t.head, h)
      val h1count = hcount * 2
      t.tail.uncons match {
        case ev @ AEmpty2() =>
          // FIXME: Throw exception?
          // assert(tfactor == 1)
          new PreComposeBalancer(h1count, h1 +: ev.unsubst[AList[F, ?, B]](AList.empty[F, B]))
        case AJust2(f, fs) =>
          add(h1, f +: fs, h1count, tfactor / 2)
      }
    }
}

object PreComposeBalancer {

  def apply[F[_, _], A, B](f: F[A, B]): PreComposeBalancer[F, A, B] =
    new PreComposeBalancer(1, AList1(f))

  def leftAction[F[_, _], Z](implicit F: Compose[F]): LeftAction[PreComposeBalancer[F, ?, Z], F] =
    ν[LeftAction[PreComposeBalancer[F, ?, Z], F]][X, Y]((f, acc) => f +: acc)

  def leftAction[G[_, _], F[_, _], Z](φ: F ~~> G)(implicit G: Compose[G]): LeftAction[PreComposeBalancer[G, ?, Z], F] =
    ν[LeftAction[PreComposeBalancer[G, ?, Z], F]][X, Y]((f, acc) => φ.apply(f) +: acc)
}

/**
 * Binary counter-like accumulator for type-aligned binary type constructors,
 * with the most significant bit on the left and addition of new elements (i.e. "increment") from the right.
 */
final class PostComposeBalancer[F[_, _], A, B](private val repr: PreComposeBalancer[λ[(α, β) => F[β, α]], B, A])
    extends AnyVal {
  import PostComposeBalancer._

  /** Post-compose an element. */
  def :+[C](f: F[B, C])(implicit F: Compose[F]): PostComposeBalancer[F, A, C] =
    wrap((f +: repr)(flip(F)))

  def result(implicit F: Compose[F]): F[A, B] =
    repr.result(flip(F))
}

object PostComposeBalancer {

  def apply[F[_, _], A, B](f: F[A, B]): PostComposeBalancer[F, A, B] =
    wrap(PreComposeBalancer[λ[(α, β) => F[β, α]], B, A](f))

  def wrap[F[_, _], A, B](pre: PreComposeBalancer[λ[(α, β) => F[β, α]], B, A]): PostComposeBalancer[F, A, B] =
    new PostComposeBalancer[F, A, B](pre)

  def rightAction[F[_, _], A](implicit F: Compose[F]): RightAction[PostComposeBalancer[F, A, ?], F] =
    ν[RightAction[PostComposeBalancer[F, A, ?], F]][B, C]((acc, f) => acc :+ f)

  def rightAction[G[_, _], F[_, _], A](
    φ: F ~~> G
  )(implicit G: Compose[G]): RightAction[PostComposeBalancer[G, A, ?], F] =
    ν[RightAction[PostComposeBalancer[G, A, ?], F]][B, C]((acc, f) => acc :+ φ.apply(f))

  private def flip[F[_, _]](F: Compose[F]): Compose[λ[(α, β) => F[β, α]]] =
    instanceOf(new ComposeClass[λ[(α, β) => F[β, α]]] {
      def compose[A, B, C](f: F[C, B], g: F[B, A]): F[C, A] =
        F.compose(g, f)
    })
}
