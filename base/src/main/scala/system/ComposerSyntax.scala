package scalaz
package system

import scala.language.implicitConversions
import Composer._

trait ComposerSyntax {
  implicit def composerOps[XS, O](xs: XS)(implicit XS: Ops[XS, O]): Ops[XS, O] = XS
}
