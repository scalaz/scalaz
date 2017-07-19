package scalaz

import typeclass._

class BaseHierarchy extends BaseHierarchy.BH0

object BaseHierarchy {
  trait BH0 extends BH1 {
    implicit def choiceProfunctor[P[_, _]](implicit P: Choice[P]): Profunctor[P] = P.profunctor
    implicit def monadBind[M[_]](implicit M: Monad[M]): Bind[M] = M.bind
    implicit def monadApplicative[M[_]](implicit M: Monad[M]): Applicative[M] = M.applicative
    implicit def monadApply[M[_]](implicit M: Monad[M]): Apply[M] = M.applicative.apply
    implicit def monadFunctor[M[_]](implicit M: Monad[M]): Functor[M] = M.applicative.apply.functor
    implicit def monoidSemigroup[A](implicit A: Monoid[A]): Semigroup[A] = A.semigroup
    implicit def traversableFoldable[T[_]](implicit T: Traversable[T]): Foldable[T] = T.foldable
    implicit def categoryComposable[=>:[_,_]](implicit C: Category[=>:]): Compose[=>:] = C.compose 
    implicit def comonadCobind[F[_]](implicit F: Comonad[F]): Cobind[F] = F.cobind
  }

  trait BH1 extends BH2 {
    implicit def bindApply[M[_]](implicit M: Bind[M]): Apply[M] = M.apply
    implicit def bindFunctor[M[_]](implicit M: Bind[M]): Functor[M] = M.apply.functor
    implicit def strongProfunctor[P[_, _]](implicit P: Strong[P]): Profunctor[P] = P.profunctor
  }

  trait BH2 extends BH3 {
    implicit def applicativeApply[M[_]](implicit M: Applicative[M]): Apply[M] = M.apply
    implicit def applicativeFunctor[M[_]](implicit M: Applicative[M]): Functor[M] = M.apply.functor
  }

  trait BH3 extends BH4 {
    implicit def applyFunctor[M[_]](implicit M: Apply[M]): Functor[M] = M.functor
  }

  trait BH4 {
    implicit def traversableFunctor[T[_]](implicit T: Traversable[T]): Functor[T] = T.functor
  }
}
