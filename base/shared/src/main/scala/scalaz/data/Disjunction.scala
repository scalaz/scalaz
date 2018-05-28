package scalaz
package data

import scala.Either

sealed trait Disjunction[L, R] {
  final def fold[A](la: L => A)(ra: R => A): A = this match {
    case -\/(l) => la(l)
    case \/-(r) => ra(r)
  }
}

object Disjunction extends DisjunctionInstances with DisjunctionFunctions {
  object Syntax extends DisjunctionSyntax

  type \/[L, R] = Disjunction[L, R]

  case class -\/[L, R](value: L) extends (L \/ R)
  case class \/-[L, R](value: R) extends (L \/ R)

  def swap[L, R](ab: L \/ R): R \/ L = ab.fold[R \/ L](\/-(_))(-\/(_))

  def fromEither[L, R](ab: Either[L, R]): L \/ R = ab.fold(-\/(_), \/-(_))
}
