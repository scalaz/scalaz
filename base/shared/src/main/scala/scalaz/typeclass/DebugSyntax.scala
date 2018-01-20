package scalaz
package typeclass

import language.experimental.macros

trait DebugSyntax {
  implicit final class ToDebugOps[A: Debug](self: A) {
    def debug: String = macro meta.Ops.f_0
  }
}