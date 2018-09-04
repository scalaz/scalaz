package scalaz

import scala.{ Any, AnyVal, Option, Some }

import tc._

package object data {
  val Forall: ForallModule = ForallImpl
  val ∀ : Forall.type      = Forall

  type Forall[F[_]] = Forall.Forall[F]
  type ∀[F[_]]      = Forall[F]

  val Forall2: Forall2Module = Forall2Impl
  val ∀∀ : Forall2.type      = Forall2

  type Forall2[F[_, _]] = Forall2.Forall2[F]
  type ∀∀[F[_, _]]      = Forall2[F]

  val Forall3: Forall3Module = Forall3Impl
  val ∀∀∀ : Forall3.type     = Forall3

  type Forall3[F[_, _, _]] = Forall3.Forall3[F]
  type ∀∀∀[F[_, _, _]]     = Forall3[F]

  type ~>[F[_], G[_]]               = ∀[λ[α => F[α] => G[α]]]
  type ~~>[F[_, _], G[_, _]]        = ∀∀[λ[(α, β) => F[α, β] => G[α, β]]]
  type ~~~>[F[_, _, _], G[_, _, _]] = ∀∀∀[λ[(α, β, C) => F[α, β, C] => G[α, β, C]]]

  type \/[L, R] = data.Disjunction[L, R]
  val (\/): data.Disjunction.type = data.Disjunction

  type \&/[A, B] = data.These[A, B]

  val Void: VoidModule = VoidImpl
  type Void = Void.Void

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

    def scompose[F[_, _], A](implicit F: Semicategory[F]): RightAction[F[A, ?], F] =
      ν[RightAction[F[A, ?], F]][α, β]((fa, f) => F.compose(f, fa))

    def scomposeMap[F[_, _], G[_, _], A](trans: F ~~> G)(implicit G: Semicategory[G]): RightAction[G[A, ?], F] =
      ν[RightAction[G[A, ?], F]][α, β]((fa, f) => G.compose(trans.apply(f), fa))

    implicit class Ops[G[_], F[_, _]](val action: RightAction[G, F]) extends AnyVal {
      def apply[A, B](g: G[A], f: F[A, B]): G[B] = action.apply(g, f)
    }
  }

  /** Type-aligned left action of `F` on `G`. */
  type LeftAction[G[_], F[_, _]] = Forall2.Prototype[λ[(α, β) => (F[α, β], G[β]) => G[α]]]

  object LeftAction {

    def fromRight[G[_], F[_, _]](act: RightAction[G, F]): LeftAction[G, λ[(α, β) => F[β, α]]] =
      ν[LeftAction[G, λ[(α, β) => F[β, α]]]][α, β]((f, g) => act.apply(g, f))

    def scompose[F[_, _], Z](implicit F: Semicategory[F]): LeftAction[F[?, Z], F] =
      ν[LeftAction[F[?, Z], F]][α, β]((f, fy) => F.compose(fy, f))

    def scomposeMap[F[_, _], G[_, _], Z](trans: F ~~> G)(implicit G: Semicategory[G]): LeftAction[G[?, Z], F] =
      ν[LeftAction[G[?, Z], F]][α, β]((f, fy) => G.compose(fy, trans.apply(f)))

    implicit class Ops[G[_], F[_, _]](val action: LeftAction[G, F]) extends AnyVal {
      def apply[A, B](f: F[A, B], g: G[B]): G[A] = action.apply(f, g)
    }
  }

  type ACons[F[_, _], G[_, _], α, β, γ] = (F[β, γ], G[α, β]) => G[α, γ]
  type ASnoc[F[_, _], G[_, _], α, β, γ] = (G[β, γ], F[α, β]) => G[α, γ]
  type AComposition[F[_, _], α, β, γ]   = (F[β, γ], F[α, β]) => F[α, γ]

  val AFix: AFixModule = AFixImpl
  type AFix[F[_[_, _], _, _], A, B] = AFix.AFix[F, A, B]

  val AList: AListModule = AListImpl
  type AList[F[_, _], A, B] = AList.AList[F, A, B]

  val Biconst: BiconstModule = BiconstImpl
  type Biconst[A, B, C] = Biconst.Biconst[A, B, C]

  val Compose: ComposeModule = ComposeImpl
  type Compose[F[_], G[_], X] = Compose.Compose[F, G, X]

  val Const: ConstModule = ConstImpl
  type Const[A, B] = Const.Const[A, B]

  val Cord: CordModule = CordImpl
  type Cord = Cord.Cord

  val Endo: EndoModule = EndoImpl
  type Endo[=>:[_, _], A] = Endo.Endo[=>:, A]

  val Fix: FixModule = FixImpl
  type Fix[F[_]] = Fix.Fix[F]

  val Identity: IdentityModule = IdentityImpl
  type Identity[A] = Identity.Identity[A]

  val Kleisli: KleisliModule = KleisliImpl
  type Kleisli[F[_], A, B] = Kleisli.Kleisli[F, A, B]

  val IList: IListModule = IListImpl
  type IList[A] = IList.IList[A]

  val Maybe: MaybeModule = MaybeImpl
  type Maybe[A] = Maybe.Maybe[A]

  val Maybe2: Maybe2Module = Maybe2Impl
  type Maybe2[A, B] = Maybe2.Maybe2[A, B]

  val FixFree: FixFreeModule = FixFreeImpl
  type FixFree[F[_], A] = FixFree.FixFree[F, A]
}
