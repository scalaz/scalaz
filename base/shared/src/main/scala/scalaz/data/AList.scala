package scalaz
package data

import Prelude._
import scala.annotation.tailrec
import scalaz.typeclass.{Category, Compose}

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
sealed abstract class AList[F[_, _], A, B] {
  import AList._

  def ::[Z](fza: F[Z, A]): AList[F, Z, B] = ACons(fza, this)

  def +:[Z](fza: F[Z, A]): AList1[F, Z, B] = ACons1(fza, this)

  def :::[Z](that: AList[F, Z, A]): AList[F, Z, B] =
    that.reverse reverse_::: this

  def reverse: Composed[F, A, B] = {
    @tailrec def go(p: APair[AList[F, ?, B], Composed[F, A, ?]]): Composed[F, A, B] = {
      val (fs, acc) = (p._1, p._2)
      fs match {
        case ACons(h, t)  => go(APair.of[AList[F, ?, B], Composed[F, A, ?]](t, h :: acc))
        case nil @ ANil() => nil.subst[Composed[F, A, ?]](acc)
      }
    }
    go(APair.of[AList[F, ?, B], Composed[F, A, ?]](this, empty[λ[(α, β) => F[β, α]], A]))
  }

  def reverse_:::[Z](that: Composed[F, Z, A]): AList[F, Z, B] = {
    @tailrec def go[G[_, _]](p: APair[AList[G, ?, Z], Composed[G, B, ?]]): Composed[G, B, Z] = {
      val (gs, acc) = (p._1, p._2)
      gs match {
        case ACons(g, gs) => go(APair.of[AList[G, ?, Z], Composed[G, B, ?]](gs, g :: acc))
        case nil @ ANil() => nil.subst[Composed[G, B, ?]](acc)
      }
    }
    go[λ[(α, β) => F[β, α]]](APair.of[AList[λ[(α, β) => F[β, α]], ?, Z], Composed[λ[(α, β) => F[β, α]], B, ?]](that, this))
  }

  def foldLeft[G[_]](ga: G[A])(φ: RightAction[G, F]): G[B] = {
    @tailrec def go(p: APair[G, AList[F, ?, B]]): G[B] = {
      val (g, fs) = (p._1, p._2)
      fs match {
        case ACons(h, t)  => go(APair.of[G, AList[F, ?, B]](φ.apply(g, h), t))
        case nil @ ANil() => nil.subst(g)
      }
    }
    go(APair.of[G, AList[F, ?, B]](ga, this))
  }

  def foldRight[G[_]](gb: G[B])(φ: LeftAction[G, F]): G[A] =
    reverse.foldLeft(gb)(RightAction.fromLeft(φ))

  /**
   * Compose the elements of this list in a balanced binary fashion.
   */
  def fold(implicit F: Category[F]): F[A, B] =
    this match {
      case ACons(h, t)  => t.foldLeft[PostComposeBalancer[F, A, ?]](PostComposeBalancer(h))(PostComposeBalancer.rightAction).result
      case nil @ ANil() => nil.subst[F[A, ?]](F.id[A])
    }

  /**
   * Compose the elements of this list in a balanced binary fashion.
   */
  def foldMaybe(implicit F: Compose[F]): AMaybe[F, A, B] =
    this match {
      case ACons(h, t)  => AJust(t.foldLeft[PostComposeBalancer[F, A, ?]](PostComposeBalancer(h))(PostComposeBalancer.rightAction).result)
      case nil @ ANil() => nil.subst[AMaybe[F, A, ?]](AMaybe.empty[F, A])
    }

  /**
   * Map and then compose the elements of this list in a balanced binary fashion.
   */
  def foldMap[G[_, _]](φ: F ~~> G)(implicit G: Category[G]): G[A, B] =
    this match {
      case ACons(h, t)  => t.foldLeft[PostComposeBalancer[G, A, ?]](PostComposeBalancer(φ.apply(h)))(PostComposeBalancer.rightAction(φ)).result
      case nil @ ANil() => nil.subst[G[A, ?]](G.id[A])
    }

  /**
   * Map and then compose the elements of this list in a balanced binary fashion.
   */
  def foldMapMaybe[G[_, _]](φ: F ~~> G)(implicit G: Compose[G]): AMaybe[G, A, B] =
    this match {
      case ACons(h, t)  => AJust(t.foldLeft[PostComposeBalancer[G, A, ?]](PostComposeBalancer(φ.apply(h)))(PostComposeBalancer.rightAction(φ)).result)
      case nil @ ANil() => nil.subst[AMaybe[G, A, ?]](AMaybe.empty[G, A])
    }

  def map[G[_, _]](φ: F ~~> G): AList[G, A, B] =
    foldRight[AList[G, ?, B]](empty[G, B])(ν[LeftAction[AList[G, ?, B], F]][α, β]((f, gs) => φ.apply(f) :: gs))

  def flatMap[G[_, _]](φ: F ~~> AList[G, ?, ?]): AList[G, A, B] =
    foldRight[AList[G, ?, B]](empty[G, B])(ν[LeftAction[AList[G, ?, B], F]][α, β]((f, gs) => φ.apply(f) ::: gs))

  def size: Int = {
    @tailrec def go(acc: Int, fs: AList[F, _, _]): Int = fs match {
      case ACons(_, t) => go(acc + 1, t)
      case ANil()      => acc
    }
    go(0, this)
  }

  override def toString: String =
    mkString("AList(", ", ", ")")

  def mkString(prefix: String, delim: String, suffix: String): String =
    this match {
      case ANil()      => s"$prefix$suffix"
      case ACons(h, t) =>
        val sb = new StringBuilder(prefix)
        type SB[a] = StringBuilder
        t.foldLeft[SB]( sb.append(h.toString)
                     )( ν[RightAction[SB, F]][α, β]((buf, f) => buf.append(delim).append(f.toString))
                     ).append(suffix).toString
  }
}

final case class ACons[F[_, _], A, X, B](head: F[A, X], tail: AList[F, X, B]) extends AList[F, A, B] {
  final type Pivot = X
}

sealed abstract case class ANil[F[_, _], A, B]() extends AList[F, A, B] {
  def   subst[G[_]](ga: G[A]): G[B]
  def unsubst[G[_]](gb: G[B]): G[A]
  def leibniz: A === B = subst[A === ?](Leibniz.refl[A])
}

object AList {
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

  def apply[F[_, _], A](): AList[F, A, A] = empty
  def apply[F[_, _], A, B](f: F[A, B]): AList[F, A, B] = f :: empty
  def empty[F[_, _], A]: AList[F, A, A] = Nil.asInstanceOf[AList[F, A, A]]

  private val Nil = nil[Nothing, Nothing]
  private def nil[F[_, _], A]: ANil[F, A, A] = new ANil[F, A, A] {
    def   subst[G[_]](ga: G[A]): G[A] = ga
    def unsubst[G[_]](gb: G[A]): G[A] = gb
  }
}
