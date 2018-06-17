package scalaz
package core

import java.lang.RuntimeException

import scala.{ inline, Nothing }

import com.github.ghik.silencer.silent

import algebra.SemigroupClass
import debug.DebugClass
import types.{ As, Is }

trait VoidModule {
  type Void

  def absurd[A](v: Void): A

  def unsafeVoid: Void

  def isNothing: Void === Nothing

  def conforms[A]: Void <~< A
}

trait VoidFunctions {
  @inline final def absurd[A](v: Void): A = Void.absurd[A](v)
}

trait VoidSyntax {
  implicit class Ops(v: Void) {
    def absurd[A]: A = Void.absurd(v)
  }
}

// NOTE: this is some next level black compiler magic
// but without this object syntax doesn't resolve...
object VoidModule extends VoidSyntax {
  implicit def void_<~<[A]: Void <~< A = Void.conforms[A]
}

trait VoidInstances {
  implicit final val voidEq: Eq[Void] = instanceOf[EqClass[Void]]((a, b) => a.absurd)
  implicit def voidSemigroup: Semigroup[Void] =
    instanceOf(new SemigroupClass[Void] {
      override def mappend(a1: Void, a2: => Void): Void = a1
    })
  implicit val voidDebug: Debug[Void] = DebugClass.instance[Void](Void.absurd)
}

@silent
private[core] object VoidImpl extends VoidModule with VoidSyntax with VoidInstances {
  type Void = Nothing

  def absurd[A](v: Void): A = v

  private[core] final class UnsafeVoid extends RuntimeException

  def unsafeVoid: Void = throw new UnsafeVoid

  def isNothing: Void === Nothing = Is.refl[Void]

  def conforms[A]: Void <~< A = As.refl[Void]
}
