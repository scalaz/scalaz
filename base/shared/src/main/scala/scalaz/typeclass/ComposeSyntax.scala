package scalaz
package typeclass

import com.github.ghik.silencer.silent

import scala.language.implicitConversions
import scala.language.experimental.macros

trait ComposeSyntax {
  implicit def composeOps[=>:[_, _], B, C](fa: B =>: C)(implicit F: Compose[=>:]): ComposeSyntax.Ops[=>:, B, C] =
    new ComposeSyntax.Ops(fa)
}

object ComposeSyntax {
  class Ops[=>:[_, _]: Compose, B, C](@silent self: B =>: C) {
    def compose[A](f: A =>: B): A =>: C = macro meta.Ops.fa_1
  }
}
