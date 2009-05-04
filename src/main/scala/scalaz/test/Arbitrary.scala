package scalaz.test

sealed trait Arbitrary[+A] {
  val gen: Gen[A]
}

object Arbitrary {
  def arbitrary[A](g: Gen[A]) = new Arbitrary[A] {
    val gen = g
  }
}
