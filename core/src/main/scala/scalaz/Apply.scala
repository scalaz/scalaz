package scalaz

trait Apply[F[_]] {
  val functor: Functor[F]

  def fmap[A, B](f: A => B): F[A] => F[B] =
    functor.fmap(f)

  def ap[A, B](f: F[A => B]): F[A] => F[B]
}

object Apply {

}
