package scalaz

trait BaseTypeclasses {
  type Applicative[F[_]] = typeclass.Applicative[F]
  val Applicative = typeclass.Applicative
  type Apply[F[_]] = typeclass.Apply[F]
  val Apply = typeclass.Apply
  type Bind[M[_]] = typeclass.Bind[M]
  val Bind = typeclass.Bind
  type Category[=>:[_,_]] = typeclass.Category[=>:]
  type Choice[P[_,_]] = typeclass.Choice[P]
  type Cobind[F[_]] = typeclass.Cobind[F]
  type Comonad[F[_]] = typeclass.Comonad[F]
  type Compose[P[_,_]] = typeclass.Compose[P]
  type Foldable[T[_]] = typeclass.Foldable[T]
  val Foldable = typeclass.Foldable
  type Functor[F[_]] = typeclass.Functor[F]
  val Functor = typeclass.Functor
  type InvariantFunctor[F[_]] = typeclass.InvariantFunctor[F]
  type IsContravariant[F[_]] = typeclass.IsContravariant[F]
  type IsCovariant[F[_]] = typeclass.IsCovariant[F]
  type Monad[M[_]] = typeclass.Monad[M]
  val Monad = typeclass.Monad
  type Monoid[T] = typeclass.Monoid[T]
  val Monoid = typeclass.Monoid
  type Phantom[F[_]] = typeclass.Phantom[F]
  type Profunctor[F[_,_]] = typeclass.Profunctor[F]
  type Semigroup[T] = typeclass.Semigroup[T]
  val Semigroup = typeclass.Semigroup
  type Show[A] = typeclass.Show[A]
  type Strong[F[_,_]] = typeclass.Strong[F]
  type Traversable[T[_]] = typeclass.Traversable[T]
  val Traversable = typeclass.Traversable


  def Category[=>:[_,_]](implicit P: Category[=>:]): Category[=>:] = P
  def Choice[P[_,_]](implicit P: Choice[P]): Choice[P] = P
  def Cobind[F[_]](implicit F: Cobind[F]): Cobind[F] = F
  def Comonad[F[_]](implicit F: Comonad[F]): Comonad[F] = F
  def Compose[P[_,_]](implicit P: Compose[P]): Compose[P] = P
  def InvariantFunctor[F[_]](implicit F: InvariantFunctor[F]): InvariantFunctor[F] = F
  def IsContravariant[F[_]](implicit F: IsContravariant[F]): IsContravariant[F] = F
  def IsCovariant[F[_]](implicit F: IsCovariant[F]): IsCovariant[F] = F
  def Phantom[F[_]](implicit F: Phantom[F]): Phantom[F] = F
  def Profunctor[P[_,_]](implicit P: Profunctor[P]): Profunctor[P] = P
  def Show[A](implicit A: Show[A]): Show[A] = A
  def Strong[P[_,_]](implicit P: Strong[P]): Strong[P] = P
}

trait BaseData {
  type Both[A, B] = data.Both[A, B]
  type Forall2[F[_, _]] = data.Forall2.Forall2[F]
  type Forall[F[_]] = data.Forall.Forall[F]
  type Forget[A, B, C] = data.Forget[A, B, C]
  type Identity[A] = data.Identity[A]
  type Leibniz[-L, +H >: L, A >: L <: H, B >: L <: H] = data.Leibniz[L, H, A, B]
  type Liskov[-L, +H >: L, -A >: L <: H, +B >: L <: H] = data.Liskov[L, H, A, B]
  type Maybe[A] = data.Maybe[A]
  type That[A, B] = data.That[A, B]
  type This[A, B] = data.This[A, B]

  val Both = data.Both
  val That = data.That
  val This = data.This

  val \/- = data.Disjunction.\/-
  val -\/ = data.Disjunction.-\/
  val Empty = data.MaybeImpl.Empty
  val Just = data.MaybeImpl.Just
}

trait BaseDataAliases { self: BaseData =>
  type \/[L, R] = data.Disjunction.\/[L, R]
  type ===[A, B] = data.Is[A, B]
  type <~<[-A, +B] = data.As[A, B]
  type >~>[+B, -A] = data.As[A, B]

  val Forall : data.Forall.type = data.Forall
  val ∀      : data.Forall.type = data.Forall
  type ∀[F[_]]                  = data.Forall.Forall[F]

  val Forall2 : data.Forall2.type = data.Forall2
  val ∀∀      : data.Forall2.type = data.Forall2
  type ∀∀[F[_, _]]                = data.Forall2.Forall2[F]

  type \&/[A, B] = data.These[A, B]
  type Id[X] = X
}

trait AllFunctions extends data.DisjunctionFunctions
    with data.MaybeFunctions
    with typeclass.InvariantFunctorFunctions
    with typeclass.PhantomFunctions
    with typeclass.TraversableFunctions

trait AllInstances extends data.AMaybeInstances
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
    with typeclass.ShowInstances
    with typeclass.StrongInstances
    with typeclass.TraversableInstances

trait AllSyntax extends data.AsSyntax
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
    with typeclass.ShowSyntax
    with typeclass.StrongSyntax
    with typeclass.TraversableSyntax

object Prelude extends BaseHierarchy with BaseData with AllFunctions

trait LowPriority extends BaseHierarchy
  with BaseTypeclasses
  with BaseData
  with BaseDataAliases

object Scalaz extends LowPriority
  with AllFunctions
  with AllSyntax
  with AllInstances
