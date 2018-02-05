package scalaz
package typeclass

import language.experimental.macros

import data.Cord

trait DebugSyntax {
  implicit final class ToDebugOps[A: Debug](self: A) {
    def debug: Cord = macro meta.Ops.f_0
    def debugs: String = macro meta.Ops.f_0
  }
}