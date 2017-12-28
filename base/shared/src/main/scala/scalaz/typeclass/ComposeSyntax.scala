package scalaz
package typeclass

import scala.language.experimental.macros

trait ComposeSyntax {
  implicit final class ToComposeOps[=>:[_, _]: Compose, B, C](self: B =>: C) {
    def compose[A](f: A =>: B): A =>: C = macro meta.Ops.fa_1
  }
}