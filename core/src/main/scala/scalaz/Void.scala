package scalaz

import scalaz.zio.{Void => ZVoid}

trait VoidModule {
  type Void

  def absurd[A](v: Void): A
}

object VoidImpl extends VoidModule {
  type Void = ZVoid

  def absurd[A](v: Void): A = v.absurd[A]
}