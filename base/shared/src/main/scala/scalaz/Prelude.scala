package scalaz

import typeclass._
import data._

import scala.language.implicitConversions

trait Prelude  extends data.DisjunctionFunctions
                  with data.ForallSyntax
                  with data.Forall2Syntax
                  with data.IdentityTypes
                  with data.MaybeFunctions
                  with typeclass.BindFunctions
                  with typeclass.FunctorFunctions
                  with typeclass.InvariantFunctorFunctions
                  with typeclass.LiskovFunctions
                  with typeclass.TraversableFunctions {

  // Base Class
  // ==========
  type Applicative[F[_]] = typeclass.Applicative[F]
  type Apply[F[_]] = typeclass.Apply[F]
  type Bind[M[_]] = typeclass.Bind[M]
  type Foldable[T[_]] = typeclass.Foldable[T]
  type Functor[F[_]] = typeclass.Functor[F]
  type Monad[M[_]] = typeclass.Monad[M]
  type Traversable[T[_]] = typeclass.Traversable[T]
  type Profunctor[F[_,_]] = typeclass.Profunctor[F]
  type Category[=>:[_,_]] = typeclass.Category[=>:]
  type InvariantFunctor[F[_]] = typeclass.InvariantFunctor[F]
  type Comonad[F[_]] = typeclass.Comonad[F]
  type Cobind[F[_]] = typeclass.Cobind[F]
  type IsCovariant[F[_]] = typeclass.IsCovariant[F]
  type IsContravariant[F[_]] = typeclass.IsContravariant[F]

  def Applicative[F[_]](implicit F: Applicative[F]): Applicative[F] = F
  def Apply[F[_]](implicit F: Apply[F]): Apply[F] = F
  def Bind[F[_]](implicit F: Bind[F]): Bind[F] = F
  def Foldable[F[_]](implicit F: Foldable[F]): Foldable[F] = F
  def Functor[F[_]](implicit F: Functor[F]): Functor[F] = F
  def Monad[M[_]](implicit M: Monad[M]): Monad[M] = M
  def Traversable[T[_]](implicit T: Traversable[T]): Traversable[T] = T
  def Profunctor[P[_,_]](implicit P: Profunctor[P]): Profunctor[P] = P
  def Choice[P[_,_]](implicit P: Choice[P]): Choice[P] = P
  def Strong[P[_,_]](implicit P: Strong[P]): Strong[P] = P
  def Category[=>:[_,_]](implicit P: Category[=>:]): Category[=>:] = P 
  def InvariantFunctor[F[_]](implicit F: InvariantFunctor[F]): InvariantFunctor[F] = F
  def Comonad[F[_]](implicit F: Comonad[F]): Comonad[F] = F
  def Cobind[F[_]](implicit F: Cobind[F]): Cobind[F] = F
  def IsCovariant[F[_]](implicit F: IsCovariant[F]): IsCovariant[F] = F
  def IsContravariant[F[_]](implicit F: IsContravariant[F]): IsContravariant[F] = F

  // ApplicativeSyntax
  implicit def PapplicativeOpsA[A](a: A): ApplicativeSyntax.OpsA[A] = new ApplicativeSyntax.OpsA(a)

  // ApplySyntax
  implicit def PapplyOps[F[_], A](fa: F[A])(implicit F: Apply[F]): ApplySyntax.Ops[F, A] =
    new ApplySyntax.Ops(fa)

  // BindSyntax
  implicit def PbindOps[M[_], A](ma: M[A])(implicit M: Bind[M]): BindSyntax.Ops[M, A] =
    new BindSyntax.Ops(ma)

  // FoldableSyntax
  implicit def PfoldableOps[F[_], A](fa: F[A])(implicit F: Foldable[F]): FoldableSyntax.Ops[F, A] =
    new FoldableSyntax.Ops(fa)

  // FunctorSyntax
  implicit def PfunctorOps[F[_], A](fa: F[A])(implicit F: Functor[F]): FunctorSyntax.Ops[F, A] =
    new FunctorSyntax.Ops(fa)

  // MaybeSyntax
  implicit class POptionAsMaybe[A](oa: Option[A]) { def asMaybe: Maybe[A] = Maybe.fromOption(oa) }

  // TraversableSyntax
  implicit def PtraversableOps[T[_], A](ta: T[A])(implicit T: Traversable[T]): TraversableSyntax.Ops[T, A] =
    new TraversableSyntax.Ops(ta)

  implicit def PprofunctorOps[P[_,_], A, B](pab: P[A, B])(implicit P: Profunctor[P]): ProfunctorSyntax.Ops[P, A, B] = 
    new ProfunctorSyntax.Ops(pab)
  
  implicit def PchoiceOps[P[_,_], A, B](pab: P[A, B])(implicit P: Choice[P]): ChoiceSyntax.Ops[P, A, B] =
   new ChoiceSyntax.Ops(pab)

  implicit def PstrongOps[P[_,_], A, B](pab: P[A, B])(implicit P: Strong[P]): StrongSyntax.Ops[P, A, B] =
    new StrongSyntax.Ops(pab)
   
  //InvariantFunctorSyntax
  implicit def InvariantFunctorOps[F[_], A](fa: F[A])(implicit F: InvariantFunctor[F]): InvariantFunctorSyntax.Ops[F, A] =
    new InvariantFunctorSyntax.Ops(fa)
  
  implicit def CobindOps[F[_], A](fa: F[A])(implicit F: Cobind[F]): CobindSyntax.Ops[F, A] =
    new CobindSyntax.Ops(fa)

  implicit def ComonadOps[F[_], A](fa: F[A])(implicit F: Comonad[F]): ComonadSyntax.Ops[F, A] =
    new ComonadSyntax.Ops(fa)


  // Base Data
  // =========
  
  type \/[L, R] = data.Disjunction.\/[L, R]
  type ===[A, B] = data.===[A, B]
  type Identity[A] = data.Identity[A]
  type Maybe[A] = data.Maybe[A]
  type Forget[A, B, C] = data.Forget[A, B, C]

  val Forall : data.Forall.type = data.Forall
  val ∀      : data.Forall.type = data.Forall
  type Forall[F[_]]             = Forall.Forall[F]
  type ∀[F[_]]                  = Forall.Forall[F]

  val Forall2 : data.Forall2.type = data.Forall2
  val ∀∀      : data.Forall2.type = data.Forall2
  type Forall2[F[_, _]]           = Forall2.Forall2[F]
  type ∀∀[F[_, _]]                = Forall2.Forall2[F]

}

object Prelude extends Prelude
