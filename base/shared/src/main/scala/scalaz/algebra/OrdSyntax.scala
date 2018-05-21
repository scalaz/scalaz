package scalaz
package algebra

import scala.language.experimental.macros

trait OrdSyntax {
  implicit final class ToOrdOps[A](a: A) {
    def comp(f: A)(implicit ev: Ord[A]): Ordering = macro meta.Ops.ia_1
  }
}
