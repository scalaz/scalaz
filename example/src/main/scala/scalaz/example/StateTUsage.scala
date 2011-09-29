package scalaz.example

import scalaz._

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
}