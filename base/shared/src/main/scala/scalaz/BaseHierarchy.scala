package scalaz

trait BaseHierarchy extends BaseHierarchy.BH0

object BaseHierarchy {

  trait BH0 extends BH1 {
    implicit def choiceProfunctor[P[_, _]](implicit P: Choice[P]): Profunctor[P]        = instanceOf(P)
    implicit def monadBind[M[_]](implicit M: Monad[M]): Bind[M]                         = instanceOf(M)
    implicit def monadApplicative[M[_]](implicit M: Monad[M]): Applicative[M]           = instanceOf(M)
    implicit def monadApply[M[_]](implicit M: Monad[M]): Apply[M]                       = instanceOf(M)
    implicit def monadFunctor[M[_]](implicit M: Monad[M]): Functor[M]                   = instanceOf(M)
    implicit def monoidSemigroup[A](implicit A: Monoid[A]): Semigroup[A]                = instanceOf(A)
    implicit def traversableFoldable[T[_]](implicit T: Traversable[T]): Foldable[T]     = instanceOf(T)
    implicit def categoryComposable[=>:[_, _]](implicit C: Category[=>:]): Compose[=>:] = instanceOf(C)
    implicit def comonadCobind[F[_]](implicit F: Comonad[F]): Cobind[F]                 = instanceOf(F)
  }

  trait BH1 extends BH2 {
    implicit def bindApply[M[_]](implicit M: Bind[M]): Apply[M]                  = instanceOf(M)
    implicit def bindFunctor[M[_]](implicit M: Bind[M]): Functor[M]              = instanceOf(M)
    implicit def strongProfunctor[P[_, _]](implicit P: Strong[P]): Profunctor[P] = instanceOf(P)
  }

  trait BH2 extends BH3 {
    implicit def applicativeApply[M[_]](implicit M: Applicative[M]): Apply[M]     = instanceOf(M)
    implicit def applicativeFunctor[M[_]](implicit M: Applicative[M]): Functor[M] = instanceOf(M)
  }

  trait BH3 extends BH4 {
    implicit def applyFunctor[M[_]](implicit M: Apply[M]): Functor[M] = instanceOf(M)
  }

  trait BH4 {
    implicit def traversableFunctor[T[_]](implicit T: Traversable[T]): Functor[T] = instanceOf(T)
  }
}
