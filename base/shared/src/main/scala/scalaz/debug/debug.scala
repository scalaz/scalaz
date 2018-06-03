package scalaz
package debug

import scala.{ sys, AnyVal, StringContext }

import scala.annotation.implicitAmbiguous
import scala.language.experimental.macros
import scala.language.implicitConversions

import ct.ContravariantClass
import data.Cord

/** A typeclass describing types which can be meaningfully represented as a `String`.
 */
trait DebugClass[A] {

  /** Produce a [[Cord]] representation of `a`.
   */
  def debug(a: A): Cord

  /** Produce a [[String]] representation of `a`.
   *
   * This should be equivalent to `debug(a).toString`.
   */
  def debugs(a: A): String
}

object DebugClass {

  // sorry
  trait DeriveDebug[A] extends DebugClass[A] with Alt[DeriveDebug[A]] {
    override def debug(a: A): Cord = Cord(debugs(a))
  }
  trait DeriveDebugs[A] extends DebugClass[A] with Alt[DeriveDebugs[A]] {
    override def debugs(a: A): String = debug(a).toString
  }

  sealed trait Alt[A <: Alt[A]]

  /** A factory for Debug` instances from functions to [[Cord]].
   */
  def instance[A](impl: A => Cord): Debug[A] =
    instanceOf(new DebugClass[A] with DebugClass.DeriveDebugs[A] {
      def debug(a: A) = impl(a)
    })
}

trait DebugInstances {
  implicit final def contravariantDebug: Contravariant[DebugClass] =
    instanceOf(new ContravariantClass[DebugClass] {
      def contramap[A, B](r: DebugClass[A])(f: B => A): DebugClass[B] =
        new DebugClass[B] {
          override def debugs(b: B) = r.debugs(f(b))
          override def debug(b: B)  = r.debug(f(b))
        }
    })
}

trait DebugSyntax {
  implicit final class ToDebugOps[A](self: A) {
    def debug(implicit ev: Debug[A]): Cord = macro meta.Ops.i_0
    def debugs(implicit ev: Debug[A]): String = macro meta.Ops.i_0
  }

  implicit final def debugInterpolator(sc: StringContext): DebugInterpolator.Interpolator =
    new DebugInterpolator.Interpolator(sc)
}

object DebugInterpolator {
  final class HasDebug private (override val toString: String) extends AnyVal

  object HasDebug extends HasDebug0 {
    implicit def mat[A](x: A)(implicit D: Debug[A]): HasDebug =
      new HasDebug(D.debugs(x)) // todo: make this sexier
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
