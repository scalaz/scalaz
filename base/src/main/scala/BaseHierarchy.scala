package scalaz

import clazz._

class BaseHierarchy extends BaseHierarchy.BH0

object BaseHierarchy {
  trait BH0 extends BH1 {
    implicit def monadBind[M[_]](implicit M: Monad[M]): Bind[M] = M.bind
    implicit def monadApplicative[M[_]](implicit M: Monad[M]): Applicative[M] = M.applicative
    implicit def monadApply[M[_]](implicit M: Monad[M]): Apply[M] = M.applicative.apply
    implicit def monadFunctor[M[_]](implicit M: Monad[M]): Functor[M] = M.applicative.apply.functor
  }

  trait BH1 extends BH2 {
    implicit def applicativeApply[M[_]](implicit M: Applicative[M]): Apply[M] = M.apply
    implicit def applicativeFunctor[M[_]](implicit M: Applicative[M]): Functor[M] = M.apply.functor
  }

  trait BH2 {
    implicit def applyFunctor[M[_]](implicit M: Apply[M]): Functor[M] = M.functor
    implicit def traversableFunctor[T[_]](implicit T: Traversable[T]): Functor[T] = T.functor
    implicit def traversableFoldable[T[_]](implicit T: Traversable[T]): Foldable[T] = T.foldable
  }
}
