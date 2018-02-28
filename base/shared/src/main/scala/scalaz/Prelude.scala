package scalaz

import typeclass._

trait BaseTypeclasses {
  type InstanceOf[T] = InstanceOfModule.impl.InstanceOf[T]

  @inline
  final def instanceOf[T](t: T): InstanceOf[T] = InstanceOfModule.impl.instanceOf(t)

  type Applicative[F[_]]      = InstanceOf[ApplicativeClass[F]]
  type Apply[F[_]]            = InstanceOf[ApplyClass[F]]
  type Bind[M[_]]             = InstanceOf[BindClass[M]]
  type Category[=>:[_, _]]    = InstanceOf[CategoryClass[=>:]]
  type Choice[P[_, _]]        = InstanceOf[ChoiceClass[P]]
  type Cobind[F[_]]           = InstanceOf[CobindClass[F]]
  type Comonad[F[_]]          = InstanceOf[ComonadClass[F]]
  type Compose[P[_, _]]       = InstanceOf[ComposeClass[P]]
  type Foldable[T[_]]         = InstanceOf[FoldableClass[T]]
  type Functor[F[_]]          = InstanceOf[FunctorClass[F]]
  type InvariantFunctor[F[_]] = InstanceOf[InvariantFunctorClass[F]]
  type IsContravariant[F[_]]  = InstanceOf[IsContravariantClass[F]]
  type IsCovariant[F[_]]      = InstanceOf[IsCovariantClass[F]]
  type Monad[M[_]]            = InstanceOf[MonadClass[M]]
  type Monoid[T]              = InstanceOf[MonoidClass[T]]
  type Contravariant[F[_]]    = InstanceOf[ContravariantClass[F]]
  type Phantom[F[_]]          = InstanceOf[PhantomClass[F]]
  type Profunctor[F[_, _]]    = InstanceOf[ProfunctorClass[F]]
  type Semigroup[T]           = InstanceOf[SemigroupClass[T]]
  type Debug[A]               = InstanceOf[DebugClass[A]]
  type Strong[F[_, _]]        = InstanceOf[StrongClass[F]]
  type Traversable[T[_]]      = InstanceOf[TraversableClass[T]]

  final def Applicative[F[_]](implicit F: Applicative[F]): Applicative[F]                = F
  final def Apply[F[_]](implicit F: Apply[F]): Apply[F]                                  = F
  final def Bind[F[_]](implicit F: Bind[F]): Bind[F]                                     = F
  final def Category[=>:[_, _]](implicit P: Category[=>:]): Category[=>:]                = P
  final def Choice[P[_, _]](implicit P: Choice[P]): Choice[P]                            = P
  final def Cobind[F[_]](implicit F: Cobind[F]): Cobind[F]                               = F
  final def Comonad[F[_]](implicit F: Comonad[F]): Comonad[F]                            = F
  final def Compose[P[_, _]](implicit P: Compose[P]): Compose[P]                         = P
  final def Foldable[F[_]](implicit F: Foldable[F]): Foldable[F]                         = F
  final def Functor[F[_]](implicit F: Functor[F]): Functor[F]                            = F
  final def InvariantFunctor[F[_]](implicit F: InvariantFunctor[F]): InvariantFunctor[F] = F
  final def IsContravariant[F[_]](implicit F: IsContravariant[F]): IsContravariant[F]    = F
  final def IsCovariant[F[_]](implicit F: IsCovariant[F]): IsCovariant[F]                = F
  final def Monad[M[_]](implicit M: Monad[M]): Monad[M]                                  = M
  final def Monoid[T](implicit T: Monoid[T]): Monoid[T]                                  = T
  final def Phantom[F[_]](implicit F: Phantom[F]): Phantom[F]                            = F
  final def Profunctor[P[_, _]](implicit P: Profunctor[P]): Profunctor[P]                = P
  final def Semigroup[T](implicit T: Semigroup[T]): Semigroup[T]                         = T
  final def Debug[A](implicit A: Debug[A]): Debug[A]                                     = A
  final def Strong[P[_, _]](implicit P: Strong[P]): Strong[P]                            = P
  final def Traversable[T[_]](implicit T: Traversable[T]): Traversable[T]                = T
}

trait BaseData {
  type Both[A, B]                                      = data.Both[A, B]
  type Forall2[F[_, _]]                                = data.Forall2.Forall2[F]
  type Forall[F[_]]                                    = data.Forall.Forall[F]
  type Forget[A, B, C]                                 = data.Forget[A, B, C]
  type Identity[A]                                     = data.Identity[A]
  type Leibniz[-L, +H >: L, A >: L <: H, B >: L <: H]  = data.Leibniz[L, H, A, B]
  type Liskov[-L, +H >: L, -A >: L <: H, +B >: L <: H] = data.Liskov[L, H, A, B]
  type Maybe[A]                                        = data.Maybe[A]
  type That[A, B]                                      = data.That[A, B]
  type This[A, B]                                      = data.This[A, B]

  val Both = data.Both
  val That = data.That
  val This = data.This

  val \/-   = data.Disjunction.\/-
  val -\/   = data.Disjunction.-\/
  val Empty = data.MaybeImpl.Empty
  val Just  = data.MaybeImpl.Just
}

trait BaseDataAliases { self: BaseData =>
  type \/[L, R]    = data.Disjunction.\/[L, R]
  type ===[A, B]   = data.Is[A, B]
  type <~<[-A, +B] = data.As[A, B]
  type >~>[+B, -A] = data.As[A, B]

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
    extends data.DisjunctionFunctions
    with data.MaybeFunctions
    with typeclass.InvariantFunctorFunctions
    with typeclass.PhantomFunctions
    with typeclass.TraversableFunctions

trait AllInstances
    extends data.AMaybeInstances
    with data.AsInstances
    with data.ConstInstances
    with data.DisjunctionInstances
    with data.DownStarInstances
    with data.ForgetInstances
    with data.IdentityInstances
    with data.TheseInstances
    with data.UpStarInstances
    with typeclass.BindInstances
    with typeclass.ChoiceInstances
    with typeclass.CobindInstances
    with typeclass.ComonadInstances
    with typeclass.FoldableInstances
    with typeclass.IsContravariantInstances
    with typeclass.IsCovariantInstances
    with typeclass.MonadInstances
    with typeclass.MonoidInstances
    with typeclass.PhantomInstances
    with typeclass.ProfunctorInstances
    with typeclass.SemigroupInstances
    with typeclass.DebugInstances
    with typeclass.StrongInstances
    with typeclass.TraversableInstances

trait AllSyntax
    extends data.AsSyntax
    with data.DisjunctionSyntax
    with data.ForallSyntax
    with data.Forall2Syntax
    with data.MaybeSyntax
    with data.Maybe2Syntax
    with typeclass.ApplicativeSyntax
    with typeclass.ApplySyntax
    with typeclass.BindSyntax
    with typeclass.ChoiceSyntax
    with typeclass.CobindSyntax
    with typeclass.ComonadSyntax
    with typeclass.ComposeSyntax
    with typeclass.FoldableSyntax
    with typeclass.FunctorSyntax
    with typeclass.InvariantFunctorSyntax
    with typeclass.PhantomSyntax
    with typeclass.ProfunctorSyntax
    with typeclass.SemigroupSyntax
    with typeclass.DebugSyntax
    with typeclass.StrongSyntax
    with typeclass.TraversableSyntax

object Prelude extends BaseHierarchy with BaseData with AllFunctions

trait LowPriority extends BaseHierarchy with BaseTypeclasses with BaseData with BaseDataAliases

object Scalaz extends LowPriority with AllFunctions with AllSyntax with AllInstances
