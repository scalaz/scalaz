package scalaz

sealed trait DoubleW extends PimpedType[Double] {
  import Scalaz._
}

trait DoubleWs {
  implicit def DoubleTo(n: Double): DoubleW = new DoubleW {
    val value = n
  }
}
