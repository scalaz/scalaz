package scalaz
package debug

import scala.{ sys, AnyVal, StringContext }

import scalaz.ct.ContravariantClass

import scala.annotation.implicitAmbiguous
import scala.language.experimental.macros
import scala.language.implicitConversions

/** A typeclass describing types which can be meaningfully represented as a `String`.
 */
trait DebugClass[A] {

  /** Produce a `String` representation of `a`.
   */
  /* todo: when we have a `Cord`, change this to return that instead,
   * todo: and add a `def debugs(a: A): String = debug(a).toString`
   */
  def debug(a: A): String
}

object DebugClass {

  /** A factory for `Debug` instances which delegates to Scala's `.toString` method.
   */
  def fromToString[A]: Debug[A] = instanceOf[DebugClass[A]](_.toString)
}

trait DebugInstances {
  implicit final def contravariantDebug: Contravariant[DebugClass] =
    instanceOf(new ContravariantClass[DebugClass] {
      def contramap[A, B](r: DebugClass[A])(f: B => A): DebugClass[B] =
        b => r.debug(f(b))
    })
}

trait DebugSyntax {
  implicit final class ToDebugOps[A](self: A) {
    def debug(implicit ev: Debug[A]): String = macro meta.Ops.i_0
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
    @implicitAmbiguous(
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
