package scalaz.example

import scalaz._

object StateTUsage extends App {
  import StateT._

  def f[M[+_]: Functor] {
    Functor[({type l[a] = StateT[M, Int, a]})#l]
  }

  def p[M[+_]: Pointed] {
    Functor[({type l[a] = StateT[M, Int, a]})#l]
    Pointed[({type l[a] = StateT[M, Int, a]})#l]
  }

  def m[M[+_]: Monad] {
    Applicative[({type l[a] = StateT[M, Int, a]})#l]
    Monad[({type l[a] = StateT[M, Int, a]})#l]
    MonadState[({type f[s, a] = StateT[M, s, a]})#f, Int]
  }

  def state() {
    val state: State[String, Int] = State((x: String) => (x + 1, 0))
    val eval: Int = state.eval("")
    state.flatMap(_ => state)
  }
}
