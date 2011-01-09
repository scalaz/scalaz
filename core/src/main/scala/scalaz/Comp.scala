package scalaz

object Comp {
  import Scalaz._

  implicit def CompFunctor[M[_] : Functor, N[_] : Functor]: Functor[({type λ[α]=M[N[α]]})#λ] = new Functor[({type λ[α]=M[N[α]]})#λ] {
    def fmap[A, B](r: M[N[A]], f: A => B) = r ∘∘ f
  }

  implicit def CompApplicative[M[_], N[_]](implicit ma: Applicative[M], na: Applicative[N]): Applicative[({type λ[α]=M[N[α]]})#λ] = new Applicative[({type λ[α]=M[N[α]]})#λ] {
    def pure[A](a: => A): M[N[A]] = a.η[N].η[M]

    def apply[A, B](f: M[N[A => B]], a: M[N[A]]): M[N[B]] = (a <**> f)(_ <*> _)
  }
}