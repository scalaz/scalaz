package scalaz

////
////
trait MonadErrorParent[F[_], S] { self: MonadError[F, S] =>
  ////

  def emap[A, B](fa: F[A])(f: A => S \/ B): F[B] =
    syntax.monadError.ToMonadErrorOps(fa)(self).emap(f)

  ////
}
