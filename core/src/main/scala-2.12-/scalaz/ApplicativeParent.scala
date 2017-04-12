package scalaz

////
////
trait ApplicativeParent[F[_]] { self: Applicative[F] =>
  ////

  def flip: Applicative[F]

  ////
}
