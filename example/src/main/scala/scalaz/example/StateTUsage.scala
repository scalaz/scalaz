package scalaz.example

import scalaz._

object StateTUsage extends App {
  import StateT._

  val stateT: StateT[Int, Option, Int] = StateT((t: Int) => Some(0, t + 1))
  import std.Option._

  {
    // Use State.stateTPointed to lift Pointed[Option] to Pointed[[a]StateT[Int, Option, a]]
    Pointed[({type l[a] = StateT[Int, Option, a]})#l]

    // Use State.stateTPointed to lift Applicative[Option] to Applicative[[a]StateT[Int, Option, a]]
    Applicative[({type l[a] = StateT[Int, Option, a]})#l]
  }

//  implicitly[Monad[({type l[a] = StateT[Int, Option, a]})#l]]
  stateTMonadState[Int,  Option] : Monad[({type l[a] = StateT[Int, Option, a]})#l]
  
  trait M[X]
  
  locally {
    implicit val F: Functor[M] = null
    implicitly[Functor[({type l[a] = StateT[Int, M, a]})#l]]
  }

  locally {
    implicit val F: Pointed[M] = null
    implicitly[Pointed[({type l[a] = StateT[Int, M, a]})#l]]
  }

  locally {
    implicit val F: Monad[M] = null
    implicitly[Applicative[({type l[a] = StateT[Int, M, a]})#l]]
    implicitly[Monad[({type l[a] = StateT[Int, M, a]})#l]]
  }
}