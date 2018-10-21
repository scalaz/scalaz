package scalaz
package data

import java.lang.RuntimeException

import scala.{ inline, Nothing }

import com.github.ghik.silencer.silent

import prop._
import tc.{ instanceOf, Debug, DebugClass, Semigroup, SemigroupClass }

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
  implicit def voidSemigroup: Semigroup[Void] =
    instanceOf(new SemigroupClass[Void] {
      override def mappend(a1: Void, a2: Void): Void = a1
    })
  implicit val voidDebug: Debug[Void] = DebugClass.instance[Void](Void.absurd)
}

@silent
private[data] object VoidImpl extends VoidModule with VoidSyntax {
  type Void = Nothing

  def absurd[A](v: Void): A = v

  private[data] final class UnsafeVoid extends RuntimeException

  def unsafeVoid: Void = throw new UnsafeVoid

  def isNothing: Void === Nothing = Is.refl[Void]

  def conforms[A]: Void <~< A = As.refl[Void]
}
