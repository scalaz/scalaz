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

  def foo {
    import scalaz._, Scalaz._
    val traverse = List(1, 2, 3).traverseU(a => State((x: Int) => (x + 1, a))).run
    traverse: State[Int, List[Int]]
  }
}