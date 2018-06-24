package scalaz

////
import Isomorphism.{ <~>, IsoFunctorTemplate }
import Tags.Parallel

////
trait ApplicativeParent[F[_]] { self: Applicative[F] =>
  ////

  /**
   * A lawful implementation of this that is isomorphic up to the methods
   * defined on Applicative allowing for optimised parallel implementations that
   * would otherwise violate laws of more specific typeclasses (e.g. Monad).
   */
  def par: Applicative.Par[F] = Tags.Parallel.subst1[Applicative, F](self)

  ////
}
