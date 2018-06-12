package scalaz
package ct

import scala.language.experimental.macros

trait SemigroupoidClass[=>:[_, _]] {
  def compose[A, B, C](f: B =>: C, g: A =>: B): (A =>: C)
}

trait SemigroupoidSyntax {
  implicit final class ToSemigroupoidOps[=>:[_, _], B, C](self: B =>: C) {
    def compose[A](f: A =>: B)(implicit ev: Semicategory[=>:]): A =>: C = macro meta.Ops.ia_1
  }
}
