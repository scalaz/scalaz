package scalaz.test

import scalaz.{ Applicative, Traversable }
import scalaz.Scalaz._

object property {

  def exhaustive[F[_]: Applicative, T[_]: Traversable, I, O](in: T[(() => I)])(testGenerator: I => F[O]): F[T[O]] =
    Traversable[T].traverse(in)(i => testGenerator(i()))

  def lazify[I](i: I): () => I = () => i

  def exhaustiveS[F[_]: Applicative, T[_]: Traversable, I, O](in: T[I])(testGenerator: I => F[O]): F[T[O]] =
    Traversable[T].traverse(in)(testGenerator)

  // def exhaustiveV[F[_]: Applicative, I, O]
  //                (in: (() => I)*)
  //                (testGenerator: I => F[O]): F[O] =
  //   exhaustive(in.toList)(testGenerator)

}
