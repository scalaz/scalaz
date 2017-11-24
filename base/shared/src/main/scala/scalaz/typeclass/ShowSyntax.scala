package scalaz
package typeclass

import com.github.ghik.silencer.silent

import language.implicitConversions
import language.experimental.macros

trait ShowSyntax {
  implicit def showOps[A](self: A)(implicit A: Show[A]) = new ShowSyntax.Ops[A](self)(A)
}

object ShowSyntax {
  class Ops[A](@silent self: A)(implicit @silent A: Show[A]) {
    def show: String = macro meta.Ops._f0[String]
  }
}