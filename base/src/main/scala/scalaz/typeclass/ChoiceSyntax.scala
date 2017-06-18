package scalaz
package typeclass

import scala.language.implicitConversions
import scala.language.experimental.macros
import data.Disjunction._

trait ChoiceSyntax {
  implicit def choiceOps[F[_, _], A, B](fa: F[A, B])(implicit F: Choice[F]): ChoiceSyntax.Ops[F, A, B] =
    new ChoiceSyntax.Ops(fa)
}

object ChoiceSyntax {
  class Ops[P[_, _], A, B](self: P[A, B])(implicit P: Choice[P]) {
   
    def leftchoice[C]: P[A \/ C, B \/ C] = macro meta.Ops._f0[P[A \/ C, B \/ C]]

    def rightchoice[C]: P[C \/ A, C \/ B] = macro meta.Ops._f0[P[C \/ A, C \/ B]]
  }
}
