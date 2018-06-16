package scalaz.syntax

import scalaz._

trait VoidOps {
  implicit class VoidOps(v: Void) {
    def absurd[A]: A = Void.absurd(v)
  }
}
