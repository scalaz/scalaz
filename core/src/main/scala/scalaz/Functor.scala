package scalaz

import scala.Function1

/* Type-class */
trait Functor[F[_]] {
  def functor: GeneralFunctor[Function1, Function1, F]
}
