package scalaz.syntax

import scalaz._

trait VoidSyntax {
  implicit class Ops(v: Void) {
    def absurd[A]: A = Void.absurd(v)
  }
}
