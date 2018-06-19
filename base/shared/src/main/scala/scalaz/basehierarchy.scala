package scalaz

import kernel.instanceOf
import ct.FunctorClass

trait BaseHierarchy extends BaseHierarchy.BH0

object BaseHierarchy {

  trait BH0 extends BH1 {
    implicit def choiceProfunctor[P[_, _]](implicit P: Choice[P]): Profunctor[P]               = instanceOf(P)
    implicit def applyFunctor[M[_]](implicit M: Apply[M]): Functor[M]                          = instanceOf(M)
    implicit def bindApply[M[_]](implicit M: Bind[M]): Apply[M]                                = instanceOf(M)
    implicit def monadBind[M[_]](implicit M: Monad[M]): Bind[M]                                = instanceOf(M)
    implicit def monadApplicative[M[_]](implicit M: Monad[M]): Applicative[M]                  = instanceOf(M)
    implicit def monoidSemigroup[A](implicit A: Monoid[A]): Semigroup[A]                       = instanceOf(A)
    implicit def traversableFoldable[T[_]](implicit T: Traversable[T]): Foldable[T]            = instanceOf(T)
    implicit def categorySemicategory[=>:[_, _]](implicit C: Category[=>:]): Semicategory[=>:] = instanceOf(C)
    implicit def comonadCobind[F[_]](implicit F: Comonad[F]): Cobind[F]                        = instanceOf(F)
    implicit def functorInvariantFunctor[F[_]](implicit F: Functor[F]): InvariantFunctor[F]    = instanceOf(F)
  }

  trait BH1 extends BH2 {
    implicit def contravariantInvariantFunctor[F[_]](implicit F: Contravariant[F]): InvariantFunctor[F] = instanceOf(F)
    implicit def traversableFunctor[T[_]](implicit T: Traversable[T]): Functor[T]                       = instanceOf(T)
    implicit def applicativeApply[M[_]](implicit M: Applicative[M]): Apply[M]                           = instanceOf(M)
    implicit def strongProfunctor[P[_, _]](implicit P: Strong[P]): Profunctor[P]                        = instanceOf(P)
  }

  trait BH2 extends BH3 {
    implicit def bifunctorFunctor[F[_, _], A](implicit F: Bifunctor[F]): Functor[F[A, ?]] =
      instanceOf(new FunctorClass[F[A, ?]] {
        def map[B, C](fab: F[A, B])(f: B => C): F[A, C] = F.rmap(fab)(f)
      })
  }

  trait BH3 extends BH4 {
    implicit def profunctorFunctor[F[_, _], A](implicit F: Profunctor[F]): Functor[F[A, ?]] =
      instanceOf(new FunctorClass[F[A, ?]] {
        def map[B, C](fab: F[A, B])(f: B => C): F[A, C] = F.rmap(fab)(f)
      })
  }

  trait BH4 {}
}
