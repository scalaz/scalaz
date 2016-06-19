package scalaz
package data

import Disjunction.{\/, \/-, -\/}

trait DisjunctionFunctions {
  @inline def -\/[L](value: L): Disjunction[L, Nothing] = -\/(value)
  @inline def \/-[R](value: R): Disjunction[Nothing, R] = \/-(value)

  @inline def left[L](value: L): Disjunction[L, Nothing] = -\/(value)
  @inline def right[R](value: R): Disjunction[Nothing, R] = \/-(value)

  def either[A, B, C](ac: A => C)(bc: B => C): A \/ B => C = _ match {
    case -\/(l)  => ac(l)
    case \/-(r) => bc(r)
  }
}
