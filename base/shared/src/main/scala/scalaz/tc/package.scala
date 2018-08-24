package scalaz

import scala.inline

package object tc {
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
  type Contravariant[F[_]]    = InstanceOf[ContravariantClass[F]]
  type Debug[A]               = InstanceOf[DebugClass[A]]
  type Eq[A]                  = InstanceOf[EqClass[A]]
  type Foldable[T[_]]         = InstanceOf[FoldableClass[T]]
  type Functor[F[_]]          = InstanceOf[FunctorClass[F]]
  type InvariantFunctor[F[_]] = InstanceOf[InvariantFunctorClass[F]]
  type Monad[M[_]]            = InstanceOf[MonadClass[M]]
  type Monoid[T]              = InstanceOf[MonoidClass[T]]
  type MeetSemiLattice[A]     = InstanceOf[MeetSemiLatticeClass[A]]
  type JoinSemiLattice[A]     = InstanceOf[JoinSemiLatticeClass[A]]
  type Lattice[A]             = InstanceOf[LatticeClass[A]]
  type Band[A]                = InstanceOf[BindClass[A]]
  type Ord[T]                 = InstanceOf[OrdClass[T]]
  type Phantom[F[_]]          = InstanceOf[PhantomClass[F]]
  type Profunctor[F[_, _]]    = InstanceOf[ProfunctorClass[F]]
  type Semicategory[P[_, _]]  = InstanceOf[SemicategoryClass[P]]
  type Semigroup[T]           = InstanceOf[SemigroupClass[T]]
  type Strong[F[_, _]]        = InstanceOf[StrongClass[F]]
  type Traversable[T[_]]      = InstanceOf[TraversableClass[T]]
  type Unfoldable[F[_]]       = InstanceOf[UnfoldableClass[F]]

  final def Applicative[F[_]](implicit F: Applicative[F]): Applicative[F]                = F
  final def Apply[F[_]](implicit F: Apply[F]): Apply[F]                                  = F
  final def Bifunctor[F[_, _]](implicit F: Bifunctor[F]): Bifunctor[F]                   = F
  final def Bind[F[_]](implicit F: Bind[F]): Bind[F]                                     = F
  final def Band[A](implicit A: Band[A]): Band[A]                                        = A
  final def Category[=>:[_, _]](implicit P: Category[=>:]): Category[=>:]                = P
  final def Choice[P[_, _]](implicit P: Choice[P]): Choice[P]                            = P
  final def Cobind[F[_]](implicit F: Cobind[F]): Cobind[F]                               = F
  final def Comonad[F[_]](implicit F: Comonad[F]): Comonad[F]                            = F
  final def Foldable[F[_]](implicit F: Foldable[F]): Foldable[F]                         = F
  final def Functor[F[_]](implicit F: Functor[F]): Functor[F]                            = F
  final def InvariantFunctor[F[_]](implicit F: InvariantFunctor[F]): InvariantFunctor[F] = F
  final def JoinSemiLattice[A](implicit L: JoinSemiLattice[A]): JoinSemiLattice[A]       = L
  final def Lattice[A](implicit L: Lattice[A]): Lattice[A]                               = L
  final def MeetSemiLattice[A](implicit L: MeetSemiLattice[A]): MeetSemiLattice[A]       = L
  final def Monad[M[_]](implicit M: Monad[M]): Monad[M]                                  = M
  final def Monoid[T](implicit T: Monoid[T]): Monoid[T]                                  = T
  final def Ord[T](implicit T: Ord[T]): Ord[T]                                           = T
  final def Phantom[F[_]](implicit F: Phantom[F]): Phantom[F]                            = F
  final def Profunctor[P[_, _]](implicit P: Profunctor[P]): Profunctor[P]                = P
  final def Semicategory[P[_, _]](implicit P: Semicategory[P]): Semicategory[P]          = P
  final def Semigroup[T](implicit T: Semigroup[T]): Semigroup[T]                         = T
  final def Strong[P[_, _]](implicit P: Strong[P]): Strong[P]                            = P
  final def Traversable[T[_]](implicit T: Traversable[T]): Traversable[T]                = T
  final def Unfoldable[F[_]](implicit F: Unfoldable[F]): Unfoldable[F]                   = F
}
