package scalaz

import typeclass._
import data._

import scala.language.implicitConversions

trait Prelude  extends data.DisjunctionFunctions
                  with data.MaybeFunctions
                  with data.MaybeOptics {
  // Core Class
  // ==========
  type Applicative[F[_]] = typeclass.Applicative[F]
  type Apply[F[_]] = typeclass.Apply[F]
  type Bind[M[_]] = typeclass.Bind[M]
  type Foldable[T[_]] = typeclass.Foldable[T]
  type Functor[F[_]] = typeclass.Functor[F]
  type Monad[M[_]] = typeclass.Monad[M]
  type Traversable[T[_]] = typeclass.Traversable[T]

  def Applicative[F[_]](implicit F: Applicative[F]): Applicative[F] = F
  def Apply[F[_]](implicit F: Apply[F]): Apply[F] = F
  def Bind[F[_]](implicit F: Bind[F]): Bind[F] = F
  def Foldable[F[_]](implicit F: Foldable[F]): Foldable[F] = F
  def Functor[F[_]](implicit F: Functor[F]): Functor[F] = F
  def Monad[M[_]](implicit M: Monad[M]): Monad[M] = M
  def Traversable[T[_]](implicit T: Traversable[T]): Traversable[T] = T

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

  // Core Data
  // =========

  type \/[L, R] = data.Disjunction.\/[L, R]
  type Maybe[A] = data.Maybe[A]
}

object Prelude extends Prelude
