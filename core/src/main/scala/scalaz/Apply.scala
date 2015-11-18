package scalaz

import scala.Function1

/* Type-class */
trait Apply[F[_]] {
  def functor: GeneralFunctor[λ[(α, β) => F[α => β]], Function1, F]
}
