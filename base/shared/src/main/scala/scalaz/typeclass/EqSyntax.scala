package scalaz
package typeclass

trait EqSyntax {
  implicit final class ToEqOps[A: Eq](a: A) {
    // TODO: macro syntax
    def ===(b: A): Boolean = implicitly[Eq[A]].equal(a, b)
  }
}
