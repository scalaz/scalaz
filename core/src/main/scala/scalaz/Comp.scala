package scalaz

trait Comp[M[_], N[_]] {
  type Apply[A] = M[N[A]]
}

trait Comps {
  import Scalaz._

  // todo at least #2741 prevents the code below from working.
//  implicit def CompFunctor[M[_] : Functor, N[_] : Functor] = new Functor[Comp[M, N]#Apply] {
//    def fmap[A, B](r: M[N[A]], f: A => B) = r ∘∘ f
//  }

//  implicit def MofNApplicative[M[_] : Applicative, N[_] : Applicative] = new Applicative[(M of N)#of] {
//    def pure[A](a: => A): M[N[A]] = a.η[N].η[M]
//
//    def apply[A, B](f: M[N[A => B]], a: M[N[A]]): M[N[B]] = (a <**> f)(_ <*> _)
//  }
}