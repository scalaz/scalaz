package scalaz
package typeclass

import com.github.ghik.silencer.silent

import scala.language.implicitConversions
import scala.language.experimental.macros

trait ComposeSyntax {
  implicit def composeOps[=>:[_, _], A, B](fa: A =>: B)(implicit F: Compose[=>:]): ComposeSyntax.Ops[=>:, A, B] =
    new ComposeSyntax.Ops(fa)
}

object ComposeSyntax {
  class Ops[=>:[_, _]: Compose, A, B](@silent self: A =>: B) {
    def compose[C](f: B =>: C): A =>: C = macro meta.Ops._f1[B =>: C, A =>: C] 
  }
}
