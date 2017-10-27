package scalaz
package data

import scala.util.control.NonFatal

import Disjunction.{\/, \/-, -\/}

trait DisjunctionFunctions {
  @inline def left[L, R](value: L): Disjunction[L, R] = -\/(value)
  @inline def right[L, R](value: R): Disjunction[L, R] = \/-(value)

  def either[A, B, C](ac: A => C)(bc: B => C): A \/ B => C = _ match {
    case -\/(l) => ac(l)
    case \/-(r) => bc(r)
  }

  def fromTryCatchNonFatal[A](block: => A): Throwable \/ A = try {
    \/-(block)
  } catch {
    case NonFatal(t) => -\/(t)
  }
}
