package scalaz

object Comp extends Comps

trait Comps {

  implicit def CompFunctor[M[_], N[_]](implicit fr: Functor[M], gr: Functor[N]): Functor[({type λ[α]=M[N[α]]})#λ] = new Functor[({type λ[α]=M[N[α]]})#λ] {
    def fmap[A, B](f: A => B) =
      r => fr.fmap((z: N[A]) => gr.fmap((a: A) => f(a))(z))(r)
  }

  implicit def CompPointed[M[_], N[_]](implicit fp: Pointed[M], gp: Pointed[N]): Pointed[({type λ[α]=M[N[α]]})#λ] =
    new Pointed[({type λ[α]=M[N[α]]})#λ] {
      def point[A](a: => A) =
        fp.point(gp.point(a))
    }

  implicit def CompPointedFunctor[M[_], N[_]](implicit fp: PointedFunctor[M], gp: PointedFunctor[N]): PointedFunctor[({type λ[α]=M[N[α]]})#λ] = {
    implicit val fr = fp.functor
    implicit val gr = gp.functor
    implicit val fpp = fp.pointed
    implicit val gpp = gp.pointed
    PointedFunctor.pointedFunctor[({type λ[α]=M[N[α]]})#λ]
  }

  implicit def CompApplicative[M[_], N[_]](implicit fa: Applicative[M], ga: Applicative[N]): Applicative[({type λ[α]=M[N[α]]})#λ] = {
    implicit val fp = fa.pointed
    implicit val gp = ga.pointed
    Applicative.applicativePA[({type λ[α]=M[N[α]]})#λ](
      implicitly[Pointed[({type λ[α]=M[N[α]]})#λ]]
    , new Applic[({type λ[α]=M[N[α]]})#λ] {
        def applic[A, B](f: M[N[A => B]]) =
          fa.liftA2((ff: N[A => B]) => (rr: N[A]) => ga.apply(ff)(rr))(f)
      }
    )
  }

  implicit def CompApplicFunctor[M[_], N[_]](implicit fp: ApplicFunctor[M], gp: ApplicFunctor[N]): ApplicFunctor[({type λ[α]=M[N[α]]})#λ] = {
    implicit val ft = fp.functor
    implicit val gt = gp.functor
    ApplicFunctor.applicFunctor[({type λ[α]=M[N[α]]})#λ](
      new Applic[({type λ[α]=M[N[α]]})#λ] {
        def applic[A, B](f: M[N[A => B]]) =
          fp.liftA2((ff: N[A => B]) => (rr: N[A]) => gp.apply(ff)(rr))(f)
      }
    , implicitly[Functor[({type λ[α]=M[N[α]]})#λ]]
    )
  }

}