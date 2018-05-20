package scalaz
package core

trait EqInstances {
  implicit final val voidEq: Eq[Void] = instanceOf[EqClass[Void]]((a, b) => a.absurd)
}
