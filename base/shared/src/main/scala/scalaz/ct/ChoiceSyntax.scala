package scalaz
package ct

import scala.language.experimental.macros

trait ChoiceSyntax {
  implicit final class ToChoiceOps[P[_, _], A, B](self: P[A, B]) {
    def leftchoice[C](implicit ev: Choice[P]): P[A \/ C, B \/ C] = macro meta.Ops.i_0
    def rightchoice[C](implicit ev: Choice[P]): P[C \/ A, C \/ B] = macro meta.Ops.i_0
  }
}
