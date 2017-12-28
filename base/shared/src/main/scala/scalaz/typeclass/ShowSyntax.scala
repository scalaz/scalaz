package scalaz
package typeclass

import language.experimental.macros

trait ShowSyntax {
  implicit final class ToShowOps[A: Show](self: A) {
    def show: String = macro meta.Ops.f_0
  }
}