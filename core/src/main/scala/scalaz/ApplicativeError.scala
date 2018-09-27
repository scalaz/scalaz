package scalaz

trait ApplicativeError[F[_], S] extends Applicative[F]{

  ////
  trait ApplicativeErrorLaws{
    def raisedErrorsHandled[A](e: S, f: S => F[A])(implicit FEA: Equal[F[A]]): Boolean =
      FEA.equal(handleError(raiseError(e))(f), f(e))
  }

  def applicativeErrorLaws = new ApplicativeErrorLaws {}

  def raiseError[A](e:S):F[A]
  def handleError[A](fa: F[A])(f: S => F[A]): F[A]
  ////

}
