package scalaz

trait Monad[F[_]] {
  val functor: Functor[F]

  val apply: Apply[F]

  val applicative: Applicative[F]

  val bind: Bind[F]
}

object Monad {

}
