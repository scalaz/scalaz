package scalaz
package typeclass

import language.experimental.macros
import language.implicitConversions

trait DebugSyntax {
  implicit final class ToDebugOps[A: Debug](self: A) {
    def debug: String = macro meta.Ops.f_0
  }

  implicit final def debugInterpolator(sc: StringContext): DebugInterpolator.Interpolator =
    new DebugInterpolator.Interpolator(sc)
}

object DebugInterpolator {
  final class HasDebug private (override val toString: String) extends AnyVal

  object HasDebug extends HasDebug0 {
    implicit def mat[A](x: A)(implicit D: Debug[A]): HasDebug = new HasDebug(D.debug(x))
  }

  sealed abstract class HasDebug0 {
    @annotation.implicitAmbiguous(
      "Cannot use the `z` interpolator to interpolate a value of type ${A}, as no implicit Show[${A}] instance is in scope."
    )
    implicit def ambiguousDebug1[A](a: A): HasDebug =
      sys.error(s"Cannot use the `z` interpolator to interpolate ${a}, as no implicit Show instance is in scope.")
    implicit def ambiguousDebug2[A](a: A): HasDebug =
      sys.error(s"Cannot use the `z` interpolator to interpolate ${a}, as no implicit Show instance is in scope.")
  }

  final class Interpolator(val sc: StringContext) extends AnyVal {
    def z(args: HasDebug*): String = sc.s(args: _*)
  }
}
