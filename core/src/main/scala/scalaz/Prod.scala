package scalaz

object Prod {
  def ProdApplicative[M[_] : Applicative, N[_] : Applicative]: Applicative[({type λ[α]=(M[α], N[α])})#λ] = new Applicative[({type λ[α]=(M[α], N[α])})#λ] {
    import Scalaz._

    def pure[A](a: => A) = (a.η[M], a.η[N])

    def apply[A, B](f: (M[A => B], N[A => B]), a: (M[A], N[A])) = {
      lazy val fv = f
      lazy val av = a
      (av._1 <*> fv._1, av._2 <*> fv._2)
    }
  }
}
