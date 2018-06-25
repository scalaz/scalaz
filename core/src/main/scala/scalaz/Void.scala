package scalaz

import scalaz.zio.{Void => ZioVoid}

trait VoidModule {
  type Void

  def absurd[A](v: Void): A
}

object VoidImpl extends VoidModule {
  type Void = ZioVoid

  def absurd[A](v: Void): A = v.absurd[A]
}