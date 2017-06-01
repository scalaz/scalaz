package scalaz.example

import scalaz._
import scalaz.std.option._
import scalaz.syntax.all._

object MonadTransUsage extends App {

  def syntaxUsage[A, B, C](ga: Option[A], gfb: Option[MaybeT[Option, B]], f: (A, B) => Option[C]): MaybeT[Option, C] = {
    for {
      a <- ga.liftM[MaybeT]
      b <- gfb.wrapEffect
      c <- f(a, b).liftM[MaybeT]
    } yield c
  }

  def syntaxUsage2[F[_[_], _], G[_], A, B, C](ga: G[A], gfb: G[F[G, B]], f: (A, B) => G[C])(implicit F: MonadTrans[F], G: Monad[G]): F[G, C] = {
    import F.apply

    for {
      a <- ga.liftM[F]
      b <- gfb.wrapEffect
      c <- f(a, b).liftM
    } yield c
  }

}
