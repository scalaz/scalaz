package scalaz
package data

trait DisjunctionFunctions {
  @inline def -\/[L](value: L): Disjunction[L, Nothing] = Disjunction.-\/(value)
  @inline def \/-[R](value: R): Disjunction[Nothing, R] = Disjunction.\/-(value)

  @inline def left[L](value: L): Disjunction[L, Nothing] = Disjunction.-\/(value)
  @inline def right[R](value: R): Disjunction[Nothing, R] = Disjunction.\/-(value)
}
