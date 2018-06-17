package scalaz
package core

import scala.language.experimental.macros

trait EqClass[A] {
  def equal(first: A, second: A): Boolean
}

trait EqInstances {
  implicit final val voidEq: Eq[Void] = instanceOf[EqClass[Void]]((a, b) => a.absurd)
}

trait EqSyntax {
  implicit final class ToEqOps[A](a: A) {
    private[core] type equal
    def ===(f: A)(implicit ev: Eq[A]): Boolean = macro meta.Ops.nia_1[equal]
  }
}
