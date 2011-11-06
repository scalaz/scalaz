package scalaz.example

import scalaz._
import collection.immutable.List

object StateTUsage extends App {
  import StateT._

  def f[M[_]: Functor] {
    Functor[({type l[a] = StateT[Int, M, a]})#l]
  }

  def p[M[_]: Pointed] {
    Functor[({type l[a] = StateT[Int, M, a]})#l]
    Pointed[({type l[a] = StateT[Int, M, a]})#l]
  }

  def m[M[_]: Monad] {
    Applicative[({type l[a] = StateT[Int, M, a]})#l]
    Monad[({type l[a] = StateT[Int, M, a]})#l]
    MonadState[({type f[s, a] = StateT[s, M, a]})#f, Int]
  }


  def stateTraverse1 {
    import scalaz._, Scalaz._
    import State.{State, stateMonad}
    val ls = List(1, 2, 3)
    val traverseOpt: Option[List[Int]] = ls.traverse(a => Some(a))
    val traverseState: State[Int, List[Int]] = ls.traverse[({type λ[α]=State[Int, α]})#λ, Int](a => State((x: Int) => (x + 1, a)))
  }

  def stateTraverse2 {
    import scalaz._, Scalaz._
    val ls = List(1, 2, 3)
    val traverseOpt: Option[List[Int]] = ls.traverseI(a => some(a))
    val traverseState = ls.traverseI(a => State((x: Int) => (x + 1, a)))
  }
}