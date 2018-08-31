package scalaz
package data

import scala.{ sys, Either, Left, Right }

import AList.aListOps

import Predef._
import tc._

/**
 * Type-aligned list with at least 1 element.
 * Example:
 *
 * {{{
 * F[A, X], F[X, Y], F[Y, Z], F[Z, B]
 * }}}
 */
sealed abstract class AList1[F[_, _], A, B] {
  import AList1._

  type Pivot

  def head: F[A, Pivot]
  def tail: AList[F, Pivot, B]

  def ::[Z](fza: F[Z, A]): AList1[F, Z, B] =
    ACons1(fza, this.toList)

  def +:[Z](fza: F[Z, A]): AList1[F, Z, B] =
    ::(fza)

  def :+[C](fbc: F[B, C]): AList1[F, A, C] =
    this ::: AList1(fbc)

  def :::[Z](that: AList1[F, Z, A]): AList1[F, Z, B] =
    that.reverse reverse_::: this

  def :++[C](that: AList[F, B, C]): AList1[F, A, C] = {
    type FXB[X] = F[X, B]
    type FXC[X] = AList1[F, X, C]
    foldRight1[AList1[F, ?, C]](∀.mk[FXB ~> FXC].from(_ +: that))(ν[LeftAction[FXC, F]][α, β](_ :: _))
  }

  def reverse_:::[Z](that: Flipped1[F, Z, A]): AList1[F, Z, B] =
    that.toList reverse_::: this

  def reverse: Flipped1[F, A, B] =
    tail reverse_::: AList1[λ[(α, β) => F[β, α]], Pivot, A](head)

  def :::[Z](that: AList[F, Z, A]): AList1[F, Z, B] =
    (that ::: this.toList).uncons match {
      case AJust2(h, t) => ACons1(h, t)
      case AEmpty2()    => sys.error("unreachable code")
    }

  def reverse_:::[Z](that: AList.Flipped[F, Z, A]): AList1[F, Z, B] =
    (that reverse_::: this.toList).uncons match {
      case AJust2(h, t) => ACons1(h, t)
      case AEmpty2()    => sys.error("unreachable code")
    }

  def uncons: Either[F[A, B], APair[F[A, ?], AList1[F, ?, B]]] =
    tail.uncons match {
      case ev @ AEmpty2() => Left(ev.subst[F[A, ?]](head))
      case AJust2(h, t)   => Right(APair.of[F[A, ?], AList1[F, ?, B]](head, ACons1(h, t)))
    }

  def sfold(implicit F: Semicategory[F]): F[A, B] =
    sfoldBalanced

  def foldLeft[G[_]](ga: G[A])(φ: RightAction[G, F]): G[B] =
    tail.foldLeft[G](φ.apply(ga, head))(φ)

  def sfoldLeft(implicit F: Semicategory[F]): F[A, B] =
    tail.foldLeft[F[A, ?]](head)(RightAction.scompose)

  def foldLeft1[G[_]](init: F[A, ?] ~> G)(φ: RightAction[G, F]): G[B] =
    tail.foldLeft[G](init.apply(head))(φ)

  def sfoldMapLeft[G[_, _]](φ: F ~~> G)(implicit G: Semicategory[G]): G[A, B] =
    tail.foldLeft[G[A, ?]](φ.apply(head))(RightAction.scomposeMap[F, G, A](φ))

  def foldRight[G[_]](gb: G[B])(φ: LeftAction[G, F]): G[A] =
    reverse.foldLeft(gb)(RightAction.fromLeft(φ))

  def foldRight1[G[_]](init: F[?, B] ~> G)(φ: LeftAction[G, F]): G[A] =
    reverse.foldLeft1[G](init)(RightAction.fromLeft(φ))

  def sfoldRight(implicit F: Semicategory[F]): F[A, B] = {
    val rev = reverse
    rev.tail.foldLeft[F[?, B]](rev.head)(RightAction.fromLeft(LeftAction.scompose))
  }

  def sfoldMapRight[G[_, _]](φ: F ~~> G)(implicit G: Semicategory[G]): G[A, B] = {
    val rev = reverse
    rev.tail.foldLeft[G[?, B]](φ.apply(rev.head))(RightAction.fromLeft(LeftAction.scomposeMap[F, G, B](φ)))
  }

  def sfoldBalanced(implicit F: Semicategory[F]): F[A, B] =
    tail.foldLeft[PostComposeBalancer[F, A, ?]](PostComposeBalancer(head))(PostComposeBalancer.rightAction).result

  def foldMapBalanced[G[_, _]: Semicategory](trans: F ~~> G): G[A, B] =
    tail.foldLeft[PostComposeBalancer[G, A, ?]](PostComposeBalancer(trans.apply(head)))(PostComposeBalancer.rightActionMap(trans)).result

  /**
   * Map and then compose the elements of this list in a balanced binary fashion.
   */
  def sfoldMapBalanced[G[_, _]](φ: F ~~> G)(implicit G: Semicategory[G]): G[A, B] =
    tail
      .foldLeft[PostComposeBalancer[G, A, ?]](PostComposeBalancer(φ.apply(head)))(PostComposeBalancer.rightActionMap(φ))
      .result

  def map[G[_, _]](φ: F ~~> G): AList1[G, A, B] =
    ACons1(φ.apply(head), tail.map(φ))

  def flatMap[G[_, _]](φ: F ~~> AList1[G, ?, ?]): AList1[G, A, B] = {
    type FXB[X] = F[X, B]
    type GXB[X] = AList1[G, X, B]
    foldRight1[AList1[G, ?, B]](∀.mk[FXB ~> GXB].from(φ.apply(_)))(
      ν[LeftAction[AList1[G, ?, B], F]][α, β]((f, gs) => φ.apply(f) ::: gs)
    )
  }

  def size: Int = 1 + tail.size

  def toList: AList[F, A, B] = head :: tail

  override def toString: String = toList.mkString("AList1(", ", ", ")")
}

final case class ACons1[F[_, _], A, X, B](head: F[A, X], tail: AList[F, X, B]) extends AList1[F, A, B] {
  type Pivot = X
}

object AList1 {

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
   * The first list has type `AList1[=>, A, E]`, while
   * the reversed list has type `Flipped1[=>, A, E]`.
   */
  type Flipped1[F[_, _], A, B] = AList1[λ[(α, β) => F[β, α]], B, A]

  def apply[F[_, _], A, B](f: F[A, B]): AList1[F, A, B] = ACons1(f, AList.empty)
  def lift[F[_, _], A, B](f: F[A, B]): AList1[F, A, B]  = ACons1(f, AList.empty)
  def op[F[_, _], A, B](f: F[A, B]): Flipped1[F, A, B]  = apply[λ[(α, β) => F[β, α]], B, A](f)

  // AList1 is a valid free semicategory in strict languages, like Scala.
  implicit def alist1FreeSemicategory[F[_, _]]: Semicategory[AList1[F, ?, ?]] =
    instanceOf(new SemicategoryClass[AList1[F, ?, ?]] {
      def compose[A, B, C](fst: AList1[F, B, C], snd: AList1[F, A, B]): AList1[F, A, C] =
        snd :++ fst.toList
    })
}
