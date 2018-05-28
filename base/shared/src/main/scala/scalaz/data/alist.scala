package scalaz
package data

import Prelude._

import java.lang.StringBuilder

import scala.AnyVal
import scala.annotation.tailrec
import scala.language.implicitConversions

sealed trait AListModule {

  /**
   * A potentially empty type-aligned list.
   * Example:
   *
   * {{{
   * F[A, X], F[X, Y], F[Y, Z], F[Z, B]
   * }}}
   *
   * The empty case witnesses type equality between `A` and `B`.
   */
  type AList[F[_, _], A, B]

  /**
   * Reversed type-aligned list is type-aligned with flipped type constructor.
   * For example, when we reverse
   *
   * {{{
   * A => B, B => C, C => D, D => E
   * }}}
   *
   * we get
   *
   * {{{
   * D => E, C => D, B => C, A => B
   * }}}
   *
   * which is type-aligned if we flip the arrows:
   *
   * {{{
   * E <= D, D <= C, C <= B, B <= A
   * }}}
   *
   * The first list has type `AList[=>, A, E]`, while
   * the reversed list has type `Composed[=>, A, E]`.
   */
  type Composed[F[_, _], A, B] = AList[λ[(α, β) => F[β, α]], B, A]

  def empty[F[_, _], A]: AList[F, A, A]
  def cons[F[_, _], A, B, C](f: F[A, B], fs: AList[F, B, C]): AList[F, A, C]
  def uncons[F[_, _], A, B](l: AList[F, A, B]): AMaybe2[F, AList[F, ?, ?], A, B]

  def apply[F[_, _], A](): AList[F, A, A]              = empty
  def apply[F[_, _], A, B](f: F[A, B]): AList[F, A, B] = cons(f, empty)

  implicit def aListOps[F[_, _], A, B](l: data.AList[F, A, B]) = new AListOps(l)
}

final class AListOps[F[_, _], A, B](val self: AList[F, A, B]) extends AnyVal {
  import AList._

  def uncons: AMaybe2[F, AList[F, ?, ?], A, B] = AList.uncons(self)

  def ::[Z](fza: F[Z, A]): AList[F, Z, B] = AList.cons(fza, self)

  def +:[Z](fza: F[Z, A]): AList1[F, Z, B] = ACons1(fza, self)

  def :::[Z](that: AList[F, Z, A]): AList[F, Z, B] =
    that.reverse reverse_::: self

  def reverse: Composed[F, A, B] = {
    @tailrec def go[X](fs: AList[F, X, B], acc: Composed[F, A, X]): Composed[F, A, B] =
      fs.uncons match {
        case AJust2(h, t)   => go(t, h :: acc)
        case ev @ AEmpty2() => ev.subst[Composed[F, A, ?]](acc)
      }

    go(self, empty[λ[(α, β) => F[β, α]], A])
  }

  def reverse_:::[Z](that: Composed[F, Z, A]): AList[F, Z, B] = {
    @tailrec def go[G[_, _], X](gs: AList[G, X, Z], acc: Composed[G, B, X]): Composed[G, B, Z] =
      gs.uncons match {
        case AJust2(g, gs)  => go(gs, g :: acc)
        case ev @ AEmpty2() => ev.subst[Composed[G, B, ?]](acc)
      }

    go[λ[(α, β) => F[β, α]], A](that, self)
  }

  def foldLeft[G[_]](ga: G[A])(φ: RightAction[G, F]): G[B] = {
    @tailrec def go[X](g: G[X], fs: AList[F, X, B]): G[B] =
      fs.uncons match {
        case AJust2(h, t)   => go(φ.apply(g, h), t)
        case ev @ AEmpty2() => ev.subst(g)
      }

    go(ga, self)
  }

  def foldRight[G[_]](gb: G[B])(φ: LeftAction[G, F]): G[A] =
    reverse.foldLeft(gb)(RightAction.fromLeft(φ))

  /**
   * Compose the elements of this list in a balanced binary fashion.
   */
  def fold(implicit F: Category[F]): F[A, B] =
    self.uncons match {
      case AJust2(h, t) =>
        t.foldLeft[PostComposeBalancer[F, A, ?]](PostComposeBalancer(h))(PostComposeBalancer.rightAction).result
      case ev @ AEmpty2() => ev.subst[F[A, ?]](F.id[A])
    }

  /**
   * Compose the elements of this list in a balanced binary fashion.
   */
  def foldMaybe(implicit F: Compose[F]): AMaybe[F, A, B] =
    self.uncons match {
      case AJust2(h, t) =>
        AJust(t.foldLeft[PostComposeBalancer[F, A, ?]](PostComposeBalancer(h))(PostComposeBalancer.rightAction).result)
      case ev @ AEmpty2() => ev.subst[AMaybe[F, A, ?]](AMaybe.empty[F, A])
    }

  /**
   * Map and then compose the elements of this list in a balanced binary fashion.
   */
  def foldMap[G[_, _]](φ: F ~~> G)(implicit G: Category[G]): G[A, B] =
    self.uncons match {
      case AJust2(h, t) =>
        t.foldLeft[PostComposeBalancer[G, A, ?]](PostComposeBalancer(φ.apply(h)))(PostComposeBalancer.rightAction(φ))
          .result
      case ev @ AEmpty2() => ev.subst[G[A, ?]](G.id[A])
    }

  /**
   * Map and then compose the elements of this list in a balanced binary fashion.
   */
  def foldMapMaybe[G[_, _]](φ: F ~~> G)(implicit G: Compose[G]): AMaybe[G, A, B] =
    self.uncons match {
      case AJust2(h, t) =>
        AJust(
          t.foldLeft[PostComposeBalancer[G, A, ?]](PostComposeBalancer(φ.apply(h)))(PostComposeBalancer.rightAction(φ))
            .result
        )
      case ev @ AEmpty2() => ev.subst[AMaybe[G, A, ?]](AMaybe.empty[G, A])
    }

  def map[G[_, _]](φ: F ~~> G): AList[G, A, B] =
    foldRight[AList[G, ?, B]](AList.empty[G, B])(ν[LeftAction[AList[G, ?, B], F]][α, β]((f, gs) => φ.apply(f) :: gs))

  def flatMap[G[_, _]](φ: F ~~> AList[G, ?, ?]): AList[G, A, B] =
    foldRight[AList[G, ?, B]](AList.empty[G, B])(ν[LeftAction[AList[G, ?, B], F]][α, β]((f, gs) => φ.apply(f) ::: gs))

  def size: Int = {
    @tailrec def go(acc: Int, fs: AList[F, _, _]): Int = fs.uncons match {
      case AJust2(_, t) => go(acc + 1, t)
      case AEmpty2()    => acc
    }
    go(0, self)
  }

  override def toString: String =
    mkString("AList(", ", ", ")")

  def mkString(prefix: String, delim: String, suffix: String): String =
    self.uncons match {
      case AEmpty2() => s"$prefix$suffix"
      case AJust2(h, t) =>
        val sb = new StringBuilder(prefix)
        type SB[a] = StringBuilder
        t.foldLeft[SB](sb.append(h.toString))(
            ν[RightAction[SB, F]][α, β]((buf, f) => buf.append(delim).append(f.toString))
          )
          .append(suffix)
          .toString
    }
}

private[data] object AListImpl extends AListModule {
  type AList[F[_, _], A, B] = AFix[AMaybe2[F, ?[_, _], ?, ?], A, B]

  def empty[F[_, _], A]: AList[F, A, A] =
    AFix.fix[AMaybe2[F, ?[_, _], ?, ?], A, A](AMaybe2.empty[F, AFix[AMaybe2[F, ?[_, _], ?, ?], ?, ?], A])

  def cons[F[_, _], A, B, C](f: F[A, B], fs: AList[F, B, C]): AList[F, A, C] =
    AFix.fix(AMaybe2[F[?, ?], AList[F, ?, ?], A, B, C](f, fs))

  def uncons[F[_, _], A, B](l: AList[F, A, B]): AMaybe2[F, AList[F, ?, ?], A, B] =
    AFix.unfix(l)
}
