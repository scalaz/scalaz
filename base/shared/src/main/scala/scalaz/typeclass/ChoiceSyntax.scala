package scalaz
package typeclass

import scala.language.experimental.macros

trait ChoiceSyntax {
  implicit final class ToChoiceOps[P[_, _]: Choice, A, B](self: P[A, B]) {
    def leftchoice[C]: P[A \/ C, B \/ C] = macro meta.Ops.f_0
    def rightchoice[C]: P[C \/ A, C \/ B] = macro meta.Ops.f_0
  }
}