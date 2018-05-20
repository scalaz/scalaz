package scalaz
package ct

import scala.language.experimental.macros

trait ComposeSyntax {
  implicit final class ToComposeOps[=>:[_, _], B, C](self: B =>: C) {
    def compose[A](f: A =>: B)(implicit ev: Compose[=>:]): A =>: C = macro meta.Ops.ia_1
  }
}
