package scalaz

import scala.{ Any, AnyVal, Option, Some }

package object data {
  val Forall: ForallModule = ForallImpl
  val ∀ : Forall.type      = Forall

  type Forall[F[_]] = Forall.Forall[F]
  type ∀[F[_]]      = Forall[F]

  val Forall2: Forall2Module = Forall2Impl
  val ∀∀ : Forall2.type      = Forall2

  type Forall2[F[_, _]] = Forall2.Forall2[F]
  type ∀∀[F[_, _]]      = Forall2[F]

  type ~>[F[_], G[_]]        = ∀[λ[α => F[α] => G[α]]]
  type ~~>[F[_, _], G[_, _]] = ∀∀[λ[(α, β) => F[α, β] => G[α, β]]]

  /**
   * Type-aligned pair. Isomorphic to
   *
   * {{{
   * (F[A], G[A]) forSome { type A }
   * }}}
   *
   * but more robust with respect to type inference.
   */
  type APair[F[_], G[_]] = BoundedAPair[Any, F, G]

  object APair {
    def apply[F[_], G[_], A](fa: F[A], ga: G[A]): APair[F, G] =
      BoundedAPair[Any, F, G, A](fa, ga)

    def unapply[F[_], G[_]](p: APair[F, G]): Option[(F[p.Pivot], G[p.Pivot])] =
      Some((p._1, p._2))

    /** Defer specifying `A`, so that it could possibly be inferred. */
    def of[F[_], G[_]] = BoundedAPair.of[Any, F, G]
  }

  /** Type-aligned right action of `F` on `G`. */
  type RightAction[G[_], F[_, _]] = Forall2.Prototype[λ[(α, β) => (G[α], F[α, β]) => G[β]]]

  object RightAction {

    def fromLeft[G[_], F[_, _]](act: LeftAction[G, F]): RightAction[G, λ[(α, β) => F[β, α]]] =
      ν[RightAction[G, λ[(α, β) => F[β, α]]]][α, β]((g, f) => act.apply(f, g))

    def compose[F[_, _], A](implicit F: Compose[F]): RightAction[F[A, ?], F] =
      ν[RightAction[F[A, ?], F]][α, β]((fa, f) => F.compose(f, fa))

    implicit class Ops[G[_], F[_, _]](val action: RightAction[G, F]) extends AnyVal {
      def apply[A, B](g: G[A], f: F[A, B]): G[B] = action.apply(g, f)
    }
  }

  /** Type-aligned left action of `F` on `G`. */
  type LeftAction[G[_], F[_, _]] = Forall2.Prototype[λ[(α, β) => (F[α, β], G[β]) => G[α]]]

  object LeftAction {

    def fromRight[G[_], F[_, _]](act: RightAction[G, F]): LeftAction[G, λ[(α, β) => F[β, α]]] =
      ν[LeftAction[G, λ[(α, β) => F[β, α]]]][α, β]((f, g) => act.apply(g, f))

    def compose[F[_, _], Z](implicit F: Compose[F]): LeftAction[F[?, Z], F] =
      ν[LeftAction[F[?, Z], F]][α, β]((f, fy) => F.compose(fy, f))

    implicit class Ops[G[_], F[_, _]](val action: LeftAction[G, F]) extends AnyVal {
      def apply[A, B](f: F[A, B], g: G[B]): G[A] = action.apply(f, g)
    }
  }

  val AFix: AFixModule = AFixImpl
  type AFix[F[_[_, _], _, _], A, B] = AFix.AFix[F, A, B]

  val AList: AListModule = AListImpl
  type AList[F[_, _], A, B] = AList.AList[F, A, B]

  val Fix: FixModule = FixImpl
  type Fix[F[_]] = Fix.Fix[F]

  val IList: IListModule = IListImpl
  type IList[A] = IList.IList[A]

  val Maybe: MaybeModule = MaybeImpl
  type Maybe[A] = Maybe.Maybe[A]

  val Maybe2: Maybe2Module = Maybe2Impl
  type Maybe2[A, B] = Maybe2.Maybe2[A, B]
}
