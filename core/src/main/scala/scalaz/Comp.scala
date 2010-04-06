package scalaz


trait Comp {
  import Scalaz._

  type of[M[_], N[_]] = PartialApplyMofN[M, N]

  trait PartialApplyMofN[M[_], N[_]] {
    type of[A] = M[N[A]]
  }

  implicit def MofNFunctor[M[_] : Functor, N[_] : Functor] = new Functor[(M of N)#of] {
    def fmap[A, B](r: M[N[A]], f: A => B) = r ∘∘ f
  }

//  implicit def MofNApplicative[M[_] : Applicative, N[_] : Applicative] = new Applicative[(M of N)#of] {
//    def pure[A](a: => A): M[N[A]] = a.η[N].η[M]
//
//    def apply[A, B](f: M[N[A => B]], a: M[N[A]]): M[N[B]] = (a <**> f)(_ <*> _)
//  }
}