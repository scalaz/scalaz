package scalaz.newtypes

trait Pimp[X] {
  val value: X
}

object Pimp extends Pimps

trait Pimps {
  implicit def Unpimp[X](n: Pimp[X]): X =
    n.value
}