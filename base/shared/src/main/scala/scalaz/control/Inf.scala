package scalaz
package control

sealed abstract class Bool
sealed abstract class True extends Bool
sealed abstract class False extends Bool

object Bool {
  sealed abstract class Value[B <: Bool]
  implicit object TrueValue extends Value[True]
  implicit object FalseValue extends Value[False]
}

/**
  * Modelled from http://www.cse.chalmers.se/~nad/publications/danielsson-altenkirch-mixing.pdf
  */
final class Inf[+A] private (thunk: () => A) {
  lazy val force: A = thunk()
}

object Inf {
  def apply[A](a: =>A): Inf[A] = new Inf(() => a)
}

sealed abstract class MaybeInf[B <: Bool, A] {
  def force: A
}
final case class Inductive[A](value: A) extends MaybeInf[True, A] {
  def force: A = value
}
final case class Coinductive[A](thunk: Inf[A]) extends MaybeInf[False, A] {
  def force: A = thunk.force
}

object MaybeInf {
  def inductive[A](a: A): MaybeInf[True, A] = Inductive(a)
  def coinductive[A](a: Inf[A]): MaybeInf[False, A] = Coinductive(a)

  def delay[B <: Bool, A](value: =>A)(implicit B: Bool.Value[B]): MaybeInf[B, A] = B match {
    case Bool.TrueValue => Inductive(value)
    case Bool.FalseValue => Coinductive(Inf(value))
  }
}
