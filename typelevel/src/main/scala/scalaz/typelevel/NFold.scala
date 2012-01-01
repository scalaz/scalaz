package scalaz
package typelevel

trait NFold[U] {

  type Zero <: U
  def zero: Zero

  type Succ[N <: U] <: U
  def succ[N <: U](n: N): Succ[N]

}

object NFold {

  final class ToInt extends NFold[Int] {

    type Zero = Int
    def zero = 0

    type Succ[N <: Int] = Int
    def succ[N <: Int](n: N) = n + 1

  }

  def toInt = new ToInt()

}

// vim: expandtab:ts=2:sw=2

