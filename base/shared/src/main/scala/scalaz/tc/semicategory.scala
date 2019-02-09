package scalaz
package tc

import scala.Predef.{ Map => SMap }
import scala.language.experimental.macros

trait SemicategoryClass[=>:[_, _]] {
  def compose[A, B, C](f: B =>: C, g: A =>: B): (A =>: C)
  final def andThen[A, B, C](g: A =>: B, f: B =>: C): (A =>: C) = compose(f, g)
}

trait SemicategorySyntax {
  implicit final class ToSemicategoryOps[=>:[_, _], B, C](self: B =>: C) {
    def compose[A](f: A =>: B)(implicit ev: Semicategory[=>:]): A =>: C = macro ops.Ops.ia_1
    def andThen[D](f: C =>: D)(implicit ev: Semicategory[=>:]): B =>: D = macro ops.Ops.ia_1
    type compose
    type andThen
    def <<<[A](f: A =>: B)(implicit ev: Semicategory[=>:]): A =>: C = macro ops.Ops.nia_1[compose]
    def >>>[D](f: C =>: D)(implicit ev: Semicategory[=>:]): B =>: D = macro ops.Ops.nia_1[andThen]
  }
}

object SemicategoryClass {
  implicit def mapSemicategory: Semicategory[SMap] =
    instanceOf(new SemicategoryClass[SMap] {
      def compose[A, B, C](f: B SMap C, g: A SMap B): A SMap C =
        g.flatMap {
          case (a, b) => f.get(b).map(c => (a, c))
        }
    })
}
