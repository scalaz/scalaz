package scalaz

import scala.Function1

// type-class
trait Functor[F[_]] {
  val functor: GeneralFunctor[Function1, Function1, F]

  def fmap[A, B](f: A => B): F[A] => F[B] =
    functor.fmap(f)

}

object Functor {

}
