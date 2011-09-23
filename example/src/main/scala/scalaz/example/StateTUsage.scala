package scalaz.example

import scalaz._

object StateTUsage extends App {
  import State._

  val stateT: StateT[Int, Option, Int] = StateT((t: Int) => Some(0, t + 1))
  import instance.Option._

  {
    // Make the parent type classes of MonadPlus implicitly available.
    implicit val P = MonadPlus[Option].monadPlusParents

    // Use State.stateTPointed to lift Pointed[Option] to Pointed[[a]StateT[Int, Option, a]]
    Pointed[({type l[a] = StateT[Int, Option, a]})#l]

    // Use State.stateTPointed to lift Applicative[Option] to Applicative[[a]StateT[Int, Option, a]]
    Applicative[({type l[a] = StateT[Int, Option, a]})#l]
  }

  implicitly[Monad[({type l[a] = StateT[Int, Option, a]})#l]]

}