package scalaz

object VoidModule {
  type Void = scalaz.zio.Void

  def absurd[A](v: Void): A = v.absurd[A]
}