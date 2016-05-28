package scalaz
package data

sealed trait Disjunction[+L, +R] {
  import Disjunction.{-\/, \/-}
  final def fold[A](la: L => A)(ra: R => A): A = this match {
    case -\/(l) => la(l)
    case \/-(r) => ra(r)
  }
}

object Disjunction extends DisjunctionInstances with DisjunctionFunctions{
  object Syntax extends DisjunctionSyntax

  type \/[L, R] = Disjunction[L, R]

  case class -\/[L](value: L) extends (L \/ Nothing)
  case class \/-[R](value: R) extends (Nothing \/ R)

  def swap[L, R](ab: L \/ R): R \/ L = ab.fold[R \/ L](\/-(_))(-\/(_))

  def fromEither[L, R](ab: Either[L, R]): L \/ R = ab.fold(-\/(_), \/-(_))
}
