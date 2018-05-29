package scalaz

import ct._
import algebra._
import scalaz.core._
import scalaz.debug._
import scalaz.types._

trait BaseTypeclasses {
  type InstanceOf[T] = InstanceOfModule.impl.InstanceOf[T]

  @inline
  final def instanceOf[T](t: T): InstanceOf[T] = InstanceOfModule.impl.instanceOf(t)

  type Applicative[F[_]]      = InstanceOf[ApplicativeClass[F]]
  type Apply[F[_]]            = InstanceOf[ApplyClass[F]]
  type Bifunctor[F[_, _]]     = InstanceOf[BifunctorClass[F]]
  type Bind[M[_]]             = InstanceOf[BindClass[M]]
  type Category[=>:[_, _]]    = InstanceOf[CategoryClass[=>:]]
  type Choice[P[_, _]]        = InstanceOf[ChoiceClass[P]]
  type Cobind[F[_]]           = InstanceOf[CobindClass[F]]
  type Comonad[F[_]]          = InstanceOf[ComonadClass[F]]
  type Compose[P[_, _]]       = InstanceOf[ComposeClass[P]]
  type Contravariant[F[_]]    = InstanceOf[ContravariantClass[F]]
  type Debug[A]               = InstanceOf[DebugClass[A]]
  type Eq[A]                  = InstanceOf[EqClass[A]]
  type Foldable[T[_]]         = InstanceOf[FoldableClass[T]]
  type Functor[F[_]]          = InstanceOf[FunctorClass[F]]
  type InvariantFunctor[F[_]] = InstanceOf[InvariantFunctorClass[F]]
  type IsContravariant[F[_]]  = InstanceOf[IsContravariantClass[F]]
  type IsCovariant[F[_]]      = InstanceOf[IsCovariantClass[F]]
  type Monad[M[_]]            = InstanceOf[MonadClass[M]]
  type Monoid[T]              = InstanceOf[MonoidClass[T]]
  type Ord[T]                 = InstanceOf[OrdClass[T]]
  type Phantom[F[_]]          = InstanceOf[PhantomClass[F]]
  type Profunctor[F[_, _]]    = InstanceOf[ProfunctorClass[F]]
  type Semigroup[T]           = InstanceOf[SemigroupClass[T]]
  type Strong[F[_, _]]        = InstanceOf[StrongClass[F]]
  type Traversable[T[_]]      = InstanceOf[TraversableClass[T]]

  final def Applicative[F[_]](implicit F: Applicative[F]): Applicative[F]                = F
  final def Apply[F[_]](implicit F: Apply[F]): Apply[F]                                  = F
  final def Bifunctor[F[_, _]](implicit F: Bifunctor[F]): Bifunctor[F]                   = F
  final def Bind[F[_]](implicit F: Bind[F]): Bind[F]                                     = F
  final def Category[=>:[_, _]](implicit P: Category[=>:]): Category[=>:]                = P
  final def Choice[P[_, _]](implicit P: Choice[P]): Choice[P]                            = P
  final def Cobind[F[_]](implicit F: Cobind[F]): Cobind[F]                               = F
  final def Comonad[F[_]](implicit F: Comonad[F]): Comonad[F]                            = F
  final def Compose[P[_, _]](implicit P: Compose[P]): Compose[P]                         = P
  final def Debug[A](implicit A: Debug[A]): Debug[A]                                     = A
  final def Eq[A](implicit P: Eq[A]): Eq[A]                                              = P
  final def Foldable[F[_]](implicit F: Foldable[F]): Foldable[F]                         = F
  final def Functor[F[_]](implicit F: Functor[F]): Functor[F]                            = F
  final def InvariantFunctor[F[_]](implicit F: InvariantFunctor[F]): InvariantFunctor[F] = F
  final def IsContravariant[F[_]](implicit F: IsContravariant[F]): IsContravariant[F]    = F
  final def IsCovariant[F[_]](implicit F: IsCovariant[F]): IsCovariant[F]                = F
  final def Monad[M[_]](implicit M: Monad[M]): Monad[M]                                  = M
  final def Monoid[T](implicit T: Monoid[T]): Monoid[T]                                  = T
  final def Ord[T](implicit T: Ord[T]): Ord[T]                                           = T
  final def Phantom[F[_]](implicit F: Phantom[F]): Phantom[F]                            = F
  final def Profunctor[P[_, _]](implicit P: Profunctor[P]): Profunctor[P]                = P
  final def Semigroup[T](implicit T: Semigroup[T]): Semigroup[T]                         = T
  final def Strong[P[_, _]](implicit P: Strong[P]): Strong[P]                            = P
  final def Traversable[T[_]](implicit T: Traversable[T]): Traversable[T]                = T
}

trait BaseCore {
  type Void = core.Void.Void

  val Void: core.Void.type = core.Void

}

trait BaseTypes {
  type Leibniz[-L, +H >: L, A >: L <: H, B >: L <: H]  = types.Leibniz[L, H, A, B]
  type Liskov[-L, +H >: L, -A >: L <: H, +B >: L <: H] = types.Liskov[L, H, A, B]

  type ===[A, B] = types.Is[A, B]
  type =!=[A, B] = types.NotIs[A, B]

  type <~<[-A, +B] = types.As[A, B]
  type >~>[+B, -A] = types.As[A, B]
}

trait BaseCt {
  type Iso[A, B]           = ct.Iso[A, B]
  type Kleisli[F[_], A, B] = ct.Kleisli[F, A, B]

  val Iso = ct.Iso

  val Kleisli: ct.Kleisli.type = ct.Kleisli

}

trait BaseAlgebra {
  val LT = algebra.LT
  val GT = algebra.GT
  val EQ = algebra.EQ

  type Ordering = algebra.Ordering
}

trait BaseData {
  type Both[A, B]       = data.Both[A, B]
  type Forall2[F[_, _]] = data.Forall2.Forall2[F]
  type Forall[F[_]]     = data.Forall.Forall[F]
  type Forget[A, B, C]  = data.Forget[A, B, C]
  type Identity[A]      = data.Identity[A]
  type Maybe[A]         = data.Maybe[A]
  type That[A, B]       = data.That[A, B]
  type This[A, B]       = data.This[A, B]

  val Both = data.Both
  val That = data.That
  val This = data.This

  val \/-   = data.Disjunction.\/-
  val -\/   = data.Disjunction.-\/
  val Empty = data.MaybeImpl.Empty
  val Just  = data.MaybeImpl.Just

}

trait BaseDataAliases {
  self: BaseData =>
  type \/[L, R] = data.Disjunction.\/[L, R]

  val Forall: data.Forall.type = data.Forall
  val ∀ : data.Forall.type     = data.Forall
  type ∀[F[_]] = data.Forall.Forall[F]

  val Forall2: data.Forall2.type = data.Forall2
  val ∀∀ : data.Forall2.type     = data.Forall2
  type ∀∀[F[_, _]] = data.Forall2.Forall2[F]

  type \&/[A, B] = data.These[A, B]
  type Id[X]     = X
}

trait AllFunctions
    extends VoidFunctions
    with data.DisjunctionFunctions
    with data.MaybeFunctions
    with ct.KleisliFunctions
    with ct.InvariantFunctorFunctions
    with ct.PhantomFunctions
    with ct.TraversableFunctions

trait AllInstances
    extends data.AMaybeInstances
    with data.ConstInstances
    with data.DisjunctionInstances
    with data.DownStarInstances
    with data.ForgetInstances
    with data.IdentityInstances
    with data.IListInstances
    with data.TheseInstances
    with data.UpStarInstances
    with std.BooleanInstances
    with std.ByteInstances
    with std.DoubleInstances
    with std.FloatInstances
    with std.IntInstances
    with std.LongInstances
    with std.ShortInstances
    with std.StringInstances
    with std.UnitInstances
    with ct.BifunctorInstances
    with ct.BindInstances
    with ct.ChoiceInstances
    with ct.CobindInstances
    with ct.ComonadInstances
    with ct.FoldableInstances
    with ct.KleisliInstances
    with ct.MonadInstances
    with ct.PhantomInstances
    with ct.ProfunctorInstances
    with ct.StrongInstances
    with ct.TraversableInstances
    with debug.DebugInstances
    with core.EqInstances
    with types.AsInstances
    with types.IsContravariantInstances
    with types.IsCovariantInstances
    with algebra.MonoidInstances
    with algebra.SemigroupInstances

trait AllSyntax
    extends VoidSyntax
    with data.DisjunctionSyntax
    with data.ForallSyntax
    with data.Forall2Syntax
    with data.MaybeSyntax
    with data.Maybe2Syntax
    with ct.ApplicativeSyntax
    with ct.ApplySyntax
    with ct.BifunctorSyntax
    with ct.BindSyntax
    with ct.ChoiceSyntax
    with ct.CobindSyntax
    with ct.ComonadSyntax
    with ct.ComposeSyntax
    with ct.FoldableSyntax
    with ct.FunctorSyntax
    with ct.InvariantFunctorSyntax
    with ct.KleisliSyntax
    with ct.PhantomSyntax
    with ct.ProfunctorSyntax
    with ct.StrongSyntax
    with ct.TraversableSyntax
    with core.EqSyntax
    with algebra.OrdSyntax
    with algebra.SemigroupSyntax
    with debug.DebugSyntax
    with types.AsSyntax

object Prelude extends BaseHierarchy with BaseData with AllFunctions

trait LowPriority
    extends BaseHierarchy
    with BaseTypeclasses
    with BaseData
    with BaseDataAliases
    with BaseTypes
    with BaseCt
    with BaseAlgebra

object Scalaz extends LowPriority with AllFunctions with AllSyntax with AllInstances
